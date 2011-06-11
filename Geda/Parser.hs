{-# OPTIONS_GHC -XRecordWildCards #-}
module Geda.Parser where

import Geda.Core
import Text.ParserCombinators.Parsec hiding (spaces,newline)
import Control.Monad (replicateM)
import Data.List (intercalate)

{- This file provides parsing utilities for gEDA schematic files
   using the Haskell Parsec monad. -}

{- For an overview of how to use Parsec, I recommend the following resources:
   
   [Tutorial] Write Yourself a Scheme in 48 Hours, Chapter 2:
   http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

   [Tutorial] Real World Haskell, Chapter 16:
   http://book.realworldhaskell.org/read/using-parsec.html

   [Reference] Hackage Parsec Documentation
   http://hackage.haskell.org/package/parsec-2.0 -}

-- Parse a GSchem File
pGSchem :: Parser [GSchem]
pGSchem = do { obs <- many pObj ; eof ; return obs }

readGSchem :: String -> Either ParseError [GSchem]
readGSchem = parse pGSchem "gschem"

-- Parse combinator for any GSchem
-- This object parses an entry in a GSchem file to a corresponding
-- object of type GSchem
pObj :: Parser GSchem
pObj = pv <|> pL <|> pG <|> pB <|> pV <|> pA <|> pT <|> pN 
       <|> pU <|> pP <|> pC <|> pH

{--- Parse a newline or eof ---}
newline :: Parser ()
newline = (char '\n' >> return ()) <|> eof <?> "newline or EOF"

{--- Parse some whitespace ---}
spaces :: Parser ()
spaces = many (char ' ') >> return ()

{--- Parse an integer (with trailing whitespace) ---}
pInt :: Parser Int
pInt = negInt <|> posInt <?> "(signed) integer"
  where
    posInt = do { ds <- many1 digit ; spaces ; return $ read ds }
    negInt = do { char '-' ; ds <- many1 digit ; spaces ; return $ - (read ds) }

{--- Parse an pair of integer (with leading whitespace) without parens ---}
pIntP :: Parser (Int,Int)
pIntP = do
  n1 <- pInt
  char ','
  n2 <- pInt
  return (n1,n2)

{--- Parse three pairs of integer (with leading whitespace) ---}
pInt3P :: Parser ((Int,Int),(Int,Int),(Int,Int))
pInt3P = do
  (n1,n2) <- pIntP
  (n3,n4) <- pIntP
  (n5,n6) <- pIntP
  return ((n1,n2),(n3,n4),(n5,n6))

{--- Parse attributes ---}
-- Parse a block of attributes to a list
pAtts :: Parser [Att]
pAtts = do 
  string "{\n" 
  atts <- many p1Att
  string "}"
  newline
  return atts 

-- Parse a single attribute; works for both GSchems and Att subtypes
p1Att :: Parser Att
p1Att = do 
  char 'T'
  spaces
  x1:y1:color:size:visibility:show_name_value:angle:alignment:
    [] <- replicateM 8 pInt
  {- num_lines_ parameter doesn't exist in legacy GSchems -}
  num_lines <- pInt <|> return (-1)
  {- How many lines we actually read depends on whether we are reading 
     legacy schematics -}
  let actual_num_lines = case num_lines of { (-1) -> 1 ; _ -> num_lines }
  newline
  key <- many (noneOf "=\n")
  char '='
  values <- replicateM actual_num_lines pLine
  let value = intercalate "\n" values
  let atts = []
  return Att {..}
  where
    pLine = do val <- many (noneOf "\n")
               newline
               return val

-- Parse combinator for the Version
pv :: Parser GSchem
pv = do 
  char 'v'
  spaces
  version <- pInt
  fileformat_version <- pInt <|> return (-1)
  newline
  return Version {..}

-- Parse combinator for a Line
pL :: Parser GSchem
pL = do 
  char 'L'
  spaces
  x1:y1:x2:y2:color:line_width:capstyle:dashstyle:dashlength:dashspace:
    [] <- replicateM 10 pInt
  newline
  atts <- pAtts <|> return []
  return $ L {..}

-- Parse combinator for a Graphic
pG :: Parser GSchem
pG = do 
  char 'G'
  spaces
  x1:y1:box_width:box_height:angle:ratio:mirrored:embedded:[] <- replicateM 11 pInt
  newline
  filename <- many (noneOf "\n")
  newline
  enc_data <- if (embedded == 1) 
    then do { enc' <- many (noneOf "\n");
              string "\n.\n";
              return enc' }
    else return ""
  atts <- pAtts <|> return [] 
  return G {..}

-- Parse combinator for a Box
pB :: Parser GSchem
pB = do 
  char 'B'
  spaces
  x1:y1:box_width:box_height:color:line_width:capstyle:dashstyle:dashlength:
    dashspace:filltype:fillwidth:angle1:pitch1:angle2:pitch2:
    [] <- replicateM 16 pInt
  newline
  atts <- pAtts <|> return [] 
  return B {..}

-- Parse combinator for a Circle
pV :: Parser GSchem
pV = do 
  char 'V'
  spaces
  x1:y1:radius:color:line_width:capstyle:dashstyle:dashlength:dashspace:
    filltype:fillwidth:angle1:pitch1:angle2:pitch2:[] <- replicateM 15 pInt
  newline
  atts <- pAtts <|> return [] 
  return $ V {..}

-- Parse combinator for an Arc
pA :: Parser GSchem
pA = do
  char 'A'
  spaces
  x1:y1:radius:startangle:sweepangle:color:line_width:capstyle:dashstyle:
    dashlength:dashspace:[] <- replicateM 11 pInt
  newline
  atts <- pAtts <|> return [] 
  return A {..}

{--- Parse a Text Object or Attribute ---}
pT :: Parser GSchem
pT = do
  char 'T'
  spaces
  x1:y1:color:size:visibility:show_name_value:angle:alignment:
    [] <- replicateM 8 pInt
  -- Same issue as in attribute parser
  num_lines <- pInt <|> return (-1)
  let actual_num_lines = case num_lines of { (-1) -> 1 ; _ -> num_lines }
  newline
  text <- replicateM actual_num_lines pToNL
  atts <- pAtts <|> return []
  -- Check if we are a floating attribute or not
  if not ((any.any) (=='=') text) then return T {..}
    else let txt = concat text in return 
         Att { key = takeWhile (/= '=') txt,
               value = drop 1 $ dropWhile (/= '=') txt,
               .. }
  where
    {--- Parse a string up to a New Line ---}
    pToNL = do ln <- many (noneOf "\n")
               newline
               return ln

-- Parse combinator for a Net
pN :: Parser GSchem
pN = do
  char 'N'
  spaces
  x1:y1:x2:y2:color:[] <- replicateM 5 pInt
  newline
  atts <- pAtts <|> return []
  return N {..}

-- Parse combinator a Bus
pU :: Parser GSchem
pU = do
  char 'U'
  spaces
  x1:y1:x2:y2:color:ripperdir:[] <- replicateM 6 pInt
  newline
  atts <- pAtts <|> return []
  return U {..}

-- Parse combinator for a Pin
pP :: Parser GSchem
pP = do
  char 'P'
  spaces
  x1:y1:x2:y2:color:[] <- replicateM 5 pInt
  {- Some schematics don't have a pintype, and whichend is calculated on
     the fly sometimes -} 
  pintype <- pInt <|> return (-1)
  whichend <- pInt <|> return (-1)
  newline
  atts <- pAtts <|> return []
  return P {..}

-- Parse combinator for a Component
pC :: Parser GSchem
pC = do
  char 'C'
  spaces
  x1:y1:selectable:angle:mirror:[] <- replicateM 5 pInt
  basename <- many (noneOf "\n")
  newline
  emb_comp <- pEmbComp <|> return []
  let sources = []
  atts <- pAtts <|> return []
  return C {..}

-- Helper parser for an embedded component
pEmbComp :: Parser [GSchem]
pEmbComp = do
  string "[\n" 
  objs <- many pObj
  char ']'
  newline
  return objs

-- Parse combinator for a GSchem patH
pH :: Parser GSchem
pH = do
  char 'H'
  spaces
  color:line_width:capstyle:dashstyle:dashlength:dashspace:filltype:fillwidth:
    angle1:pitch1:angle2:pitch2:num_lines:[] <- replicateM 13 pInt
  newline
  path <- replicateM num_lines pPath
  atts <- pAtts <|> return []
  return H {..}
  
-- Parse combinator for any Path
pPath :: Parser Path
pPath = pMM <|> pMm <|> pLL <|> pLl <|> pCC <|> pCc <|> pZ

-- Parse combinator for an (absolute) MoveTo instruction
pMM :: Parser Path
pMM = do
  char 'M'
  spaces
  moves <- many pIntP
  newline
  return $ MM moves

-- Parse combinator for a (relative) MoveTo instruction
pMm :: Parser Path
pMm = do
  char 'm'
  spaces
  moves <- many pIntP
  newline
  return $ Mm moves

-- Parse combinator for an (absolute) Line instruction
pLL :: Parser Path
pLL = do
  char 'L'
  spaces
  moves <- many pIntP
  newline
  return $ LL moves

-- Parse combinator for a (relative  Line instruction
pLl :: Parser Path
pLl = do
  char 'l'
  spaces
  moves <- many pIntP
  newline
  return $ Ll moves

-- Parse combinator for an (absolute) Bezier Curve
pCC :: Parser Path
pCC = do
  char 'C'
  spaces
  moves <- many pInt3P 
  newline
  return $ CC moves

-- Parse combinator for a (relative) Bezier Curve
pCc :: Parser Path
pCc = do
  char 'c'
  spaces
  moves <- many pInt3P 
  newline
  return $ Cc moves

-- Parse combinator for a close-path instruction
pZ :: Parser Path
pZ = do
   char 'Z' <|> char 'z'
   spaces
   newline
   return Z
