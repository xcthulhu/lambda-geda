{-# OPTIONS_GHC -XOverloadedStrings -XRecordWildCards -XFlexibleContexts #-}
module Language.VHDL.Parser where
import Text.Regex.PCRE.Light (match, compile, dotall, 
                              caseless, multiline, ungreedy)
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

{---------------------------------------}

{-- REGEX 
    In the following section we use regex to do some top 
    lever parsing, precisely because we are not parsing
    all of VHDL, which is context sensitive. Since
    we only care about a tiny fragment this approach is
    simpler and more efficient. --}

-- A simple procedure that deletes all comments from 
-- a string containing VHDL code
removeComments :: String -> String
removeComments ('-':'-':rst) = 
  removeComments $ dropWhile (/= '\n') rst
removeComments (c:rst) = c:(removeComments rst)
removeComments "" = ""

-- Extracts entity declarations from a string, while filtering comments
entityStrings :: String -> [String]
entityStrings raw = 
  map removeComments $
  map unpack $
  maybe [] id $
  match entity (pack raw) [] 
    where
      entity = compile "entity.*end.*;" [dotall,caseless,ungreedy]

-- Extracts architecture names associated with an entity 
-- identifier from a string
entityArchs :: ID -> String -> [Architecture]
entityArchs ident raw = 
  map removeComments $
  map unpack $
  maybe [] id $
  match arch (pack raw) [] 
    where
      arch = compile 
             (pack $ 
              "architecture.*of.* " 
              ++ ident 
              ++ " .*end.*;") 
             [dotall,caseless,ungreedy]

{---------------------------------------}

{-- Parsec 
    Below is a collection of parse combinators that 
    reflect extract the relevant data from preprocessed
    strings extracted by our regular expressions. --}

-- Case insensitive version of Parsec's string
string :: (Stream s m Char) => String -> ParsecT s u m String
string s = tokens ((map toLower) . show) updatePosString ((map toLower) s)

-- Extracts the name of an architecture declaration
pArchName :: Parser Architecture
pArchName = do
  spaces >> string "architecture" >> spaces
  manyTill anyChar $ 
    choice [ space >> return ()
           , eof ]

-- Reads a VHDL entity
readArchName :: String -> Either ParseError Architecture
readArchName = parse pArchName "VHDL architecture"

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
  idents <- idName `sepBy` (try $ spaces >> char ',')
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

-- Parses ports for an entity
pPort :: Parser [Port]
pPort = do
  spaces ; string "port" ; spaces ; char '('
  decs <- pPortDec `sepBy` (spaces >> char ';')
  spaces ; char ')' ; spaces ; char ';'
  return $ concat decs

-- Parses a VHDL entity
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

-- Reads a VHDL entity
readEntity :: String -> Either ParseError Entity
readEntity = parse pEntity "VHDL entity"
