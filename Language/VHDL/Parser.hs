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

-- Extracts entity declarations from a string, while filtering comments
entityStrings :: String -> [String]
entityStrings x = map removeComments $
                  map unpack $
                  maybe [] id $
                  match entity (pack x) [] 
  where
    entity = compile "entity.*end.*;" [dotall,caseless,ungreedy]
    -- No regex replace for pcre-light, so roll our own
    removeComments ('-':'-':rst) = 
      removeComments $ dropWhile (/= '\n') rst
    removeComments (c:rst) = c:(removeComments rst)
    removeComments "" = ""

-- Case insensitive version of Parsec's string
string :: (Stream s m Char) => String -> ParsecT s u m String
string s = tokens ((map toLower) . show) updatePosString ((map toLower) s)

-- Parses nested paren statements
parenstatement :: Parser String
parenstatement = do
  spaces
  v <- between (char '(') (char ')') 
               (manyTill p (lookAhead $ char ')'))
  return $ "(" ++ (concat v) ++ ")"
    where
      p = do x <- many (noneOf "()")
             y <- parenstatement <|> return []
             return $ x ++ y

-- Parses an expression, terminated by an unmatched ')' or a ';'
pExp :: Parser String
pExp = do
  spaces
  start <- many (noneOf "(;)")
  spaces
  middle <- parenstatement <|> return ""
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
  end <- parenstatement <|> return ""
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
  return Entity {..}
    
-- Reads a VHDL entity
readEntity :: String -> Either ParseError Entity
readEntity = parse pEntity "VHDL entity"