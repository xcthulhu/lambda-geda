{-# OPTIONS_GHC -XOverloadedStrings -XRecordWildCards -XFlexibleContexts #-}
module Language.VHDL.Parser where
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec.Pos (updatePosString)
import Text.ParserCombinators.Parsec hiding (string)
import Data.Char (toLower, isSpace)
import Data.ByteString.Char8 (pack, unpack)
import System.IO
import Language.VHDL.Core

{-- This module is intended to provide an incomplete VHDL 
    parser, suitable for extracting entity declarations 
    from VHDL files. --}

-- A simple procedure that deletes all comments from 
-- a string containing VHDL code
-- (huray for tail recursion)
removeComments :: String -> String
removeComments ('-':'-':rst) = 
  removeComments $ dropWhile (/= '\n') rst
removeComments (c:rst) = c:(removeComments rst)
removeComments "" = ""

{---------------------------------------}
{-- Parsec

    Below is a collection of parse combinators that 
    reflect extract the relevant data from preprocessed
    strings extracted by our regular expressions. --}

-- Case insensitive version of Parsec's string
string :: (Stream s m Char) => String -> ParsecT s u m String
string s = tokens ((map toLower) . show) updatePosString ((map toLower) s)

-- Tries to run a parser repeatedly until input is exhausted
parseAll :: Parser p -> Parser [p]
parseAll p = do
  manyTill (noneOf "") (eof <|> ((lookAhead $ try p) >> return ()))
  x <- optionMaybe (try p)
  case x of 
    Nothing -> return []
    Just a -> fmap (a :) $ parseAll p

-- Extracts a list of library inclusions
pLibs :: Parser [String]
pLibs = do
  string "library" >> spaces
  many (noneOf ",;") `sepBy` (try $ spaces >> char ',' >> spaces)

-- Reads VHDL library inclusions
readLibs :: String -> Either ParseError [String]
readLibs = (parse (fmap concat $ parseAll pLibs) 
                  "VHDL library inclusion") 
           . removeComments
           . (map toLower)

-- Extracts a list from a use directive
pUse :: Parser [String]
pUse = do
  string "use" >> spaces
  many (noneOf ",;") `sepBy` (try $ spaces >> char ',' >> spaces)

-- Reads VHDL use directives
readUses :: String -> Either ParseError [String]
readUses = (parse (fmap concat $ parseAll pUse) 
                  "VHDL use directive") 
           . removeComments
           . (map toLower)

-- Extracts the name of an architecture declaration
pArch :: String -> Parser Architecture
pArch entityID = do
  spaces >> string "architecture" >> spaces
  arc <- manyTill anyChar $ 
         choice [ space >> return (), eof ]
  spaces >> string "of" >> spaces
  string entityID
  return arc

-- Reads VHDL use directives
readArchs :: String -> String -> Either ParseError [Architecture]
readArchs entityID = (parse (parseAll $ pArch entityID) 
                            "VHDL architecture") 
                     . removeComments
                     . (map toLower)

-- Reads VHDL architectures
--readArchs :: String -> Either ParseError Architecture
--readArchs entityID = parse pArch
--                     "VHDL architecture"

-- Parses nested parenthetical statements
parenStatement :: Parser String
parenStatement = do
  spaces
  v <- between (char '(') (char ')') 
               (manyTill p (lookAhead $ char ')'))
  return $ "(" ++ (concat v) ++ ")"
    where
      p = do x <- many (noneOf "()")
             y <- parenStatement <|> return []
             return $ x ++ y

-- Parses an expression, terminated by an unmatched ')' or a ';'
pExp :: Parser String
pExp = do
  spaces
  start <- many (noneOf "(;)")
  spaces
  middle <- parenStatement <|> return ""
  end <- (lookAhead (oneOf ";)") >> return "") <|> pExp
  return $ start ++ middle ++ end

-- Parses a value assignment
pVal :: Parser Value
pVal = do
  spaces ; string ":=" ; spaces
  pExp

-- Parse an idname
idName = do
  spaces
  many $ satisfy $ 
    \x -> not $ any (\f -> f x) 
          [ isSpace, (==':'), (==',') ]

-- Parse a type
typeid = do
  spaces
  start <- many $ satisfy $ 
           \x -> not $ any (\f -> f x)
                 [ isSpace, (==';'), (=='('),
                   (==')'), (==':') ]
  spaces
  end <- parenStatement <|> return ""
  return $ start ++ end

-- Parses a generic declaration
-- Due to VHDL syntax, sometimes multiple declarations are possible
pGenDec :: Parser [(ID,Type,Maybe Value)]
pGenDec = do
  spaces
  idents <- idName `sepBy` (try $ spaces >> char ',' >> spaces)
  spaces ; char ':' ; spaces
  dectyp <- typeid
  spaces
  value <- optionMaybe pVal
  return [(i, dectyp, value) | i <- idents]

-- Parses generics for an entity
pGeneric :: Parser [Generic]
pGeneric = do
  spaces ; string "generic" ; spaces ; char '('
  decs <- pGenDec `sepBy` (spaces >> char ';')
  spaces ; char ')' ; spaces ; char ';'
  return $ concat decs

-- Parses the direction of a port
pDir :: Parser DIR
pDir = spaces >>
       choice [ string "in" >> 
                choice [ string "out" >> return INOUT 
                       , return IN ]
              , string "out" >> return OUT ]

-- Parses a port declaration
-- Due to VHDL syntax, sometimes multiple ports are declared together
pPortDec :: Parser [Port]
pPortDec = do
  spaces
  idents <- idName `sepBy` (try $ spaces >> char ',')
  spaces ; char ':'
  dir <- pDir
  dectyp <- typeid
  spaces
  value <- optionMaybe pVal
  return [(i, dir, dectyp, value) | i <- idents]

-- Parses ports for an entities
pPort :: Parser [Port]
pPort = do
  spaces ; string "port" ; spaces ; char '('
  decs <- pPortDec `sepBy` (spaces >> char ';')
  spaces ; char ')' ; spaces ; char ';'
  return $ concat decs

-- Parses a VHDL entities
pEntity :: Parser Entity
pEntity = do
  string "entity" ; spaces
  identifier <- idName
  spaces ; string "is" ; spaces
  generic <- pGeneric <|> return []
  spaces
  port <- pPort <|> return []
  return emptyEntity { identifier = identifier
                     , generic = generic 
                     , port = port }

-- Reads VHDL entities
readEntities :: String -> Either ParseError [Entity]
readEntities = (parse (parseAll pEntity) 
                      "VHDL entity") 
               . removeComments 
               . (map toLower)