{-# OPTIONS_GHC -XOverloadedStrings -XRecordWildCards -XFlexibleContexts #-}
module Language.VHDL.Parser where
import Text.Regex.PCRE.Light (match, compile, dotall, 
                              caseless, ungreedy)
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos (updatePosString)
import Text.ParserCombinators.Parsec hiding (string)
import Data.Char (toLower, isSpace)
import Data.ByteString.Char8 (pack, unpack)
import System.IO
import Language.VHDL.Core

{-- This module is intended to provide an incomplete VHDL 
    parser, suitable for extracting entity declarations 
    from VHDL files. --}

-- Extracts entity declarations from a string
entityStrings :: String -> [String]
entityStrings x = map unpack $
                  maybe [] id $
                  match regex (pack x) [] 
  where
        regex = compile "entity.*end.*;" [dotall,caseless,
                                          ungreedy]

-- Case insensitive version of Parsec's string
string :: (Stream s m Char) => String -> ParsecT s u m String
string s = tokens ((map toLower) . show) updatePosString ((map toLower) s)

-- Parses nested paren statements
parenstatement :: Parser String
parenstatement = do
  v <- between (char '(') (char ')') (many p)
  return $ "(" ++ (concat v) ++ ")"
    where
      p = do x <- many (noneOf "()")
             y <- parenstatement <|> return []
             return $ x ++ y

-- Parses an expression, terminated by an unmatched ')' or a ';'
pExp :: Parser String
pExp = do
    start <- many (noneOf "(;)")
    middle <- parenstatement <|> return []
    end <- (lookAhead (oneOf ";)") >> return []) <|> pExp
    return $ start ++ middle ++ end

-- Parses a value assignment
pVal :: Parser Value
pVal = do
    string ":=" ; spaces
    pExp

-- Parse an idname
idname = many $ satisfy $ (\x -> not ((isSpace x) || (x == ';') || (x == ':')))

-- Parse a type
typeid = do
    start <- many $ satisfy $ (\x -> not ((isSpace x) || (x == ';') ||
                                          (x == '(')  || (x == ')') ||
                                          (x == ':')))
    end <- parenstatement <|> return []
    return $ start ++ end

-- Parses a generic declaration
pGenDec :: Parser (ID,Type,Maybe Value)
pGenDec = do
    ident <- idname
    spaces ; char ':' ; spaces
    dectyp <- typeid
    spaces
    value <- optionMaybe pVal
    return (ident, dectyp, value)

-- Parses generics for an entity
pGeneric :: Parser [Generic]
pGeneric = do
    string "generic" ; spaces ; char '('
    decs <- pGenDec `sepBy` (spaces >> char ';' >> spaces)
    spaces ; char ')' ; spaces ; char ';'
    return decs

-- Parses the direction of a port
pDir :: Parser DIR
pDir = (string "in" >> return IN) <|>
       (string "out" >> return OUT) <|>
       (string "inout" >> return INOUT)

-- Parses a port declaration
pPortDec :: Parser Port
pPortDec = do
    ident <- idname
    spaces ; char ':' ; spaces
    dir <- pDir
    spaces
    dectyp <- typeid
    spaces
    value <- optionMaybe pVal
    return (ident, dir, dectyp, value)

-- Parses ports for an entity
pPort :: Parser [Port]
pPort = do
    string "port" ; spaces ; char '('
    decs <- pPortDec `sepBy` (spaces >> char ';' >> spaces)
    spaces ; char ')' ; spaces ; char ';'
    return decs

-- Parses a VHDL entity
pEntity :: Parser Entity
pEntity = do
    string "entity" ; spaces
    identifier <- idname
    spaces ; string "is" ; spaces
    generic <- pGeneric <|> return []
    spaces
    port <- pPort <|> return []
    return Entity {..}
    
-- Reads a VHDL entity
readEntity :: String -> Either ParseError Entity
readEntity = parse pEntity "VHDL entity"