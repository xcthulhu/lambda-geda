module Gaf.Parser (readGSchem) where

import Text.ParserCombinators.Parsec
import Control.Monad (replicateM,liftM)
import Gaf

{- This file provides parsing utilities for gEDA schematic files
   using the Haskell Parsec monad. -}

{- For an overview of how to use Parsec, I recommend the following resources:
   
   [Tutorial] Write Yourself a Scheme in 48 Hours, Chapter 2:
   http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

   [Tutorial] Real World Haskell, Chapter 16:
   http://book.realworldhaskell.org/read/using-parsec.html

   [Reference] Hackage Parsec Documentation
   http://hackage.haskell.org/package/parsec-2.0 -}

{--- Parse a GSchem File ---}
pGSchem :: Parser GSchem
pGSchem = do
  obs <- many pObj
  eof
  return $ foldr (:+:) Null obs

readGSchem :: String -> Either ParseError GSchem
readGSchem = parse pGSchem "gschem"

{--- Parse Any Object ---}
-- This object parses an entry in a GSchem file to a corresponding
-- object of type GSchem
pObj :: Parser GSchem
pObj = pv <|> pL <|> pG <|> pB <|> pV <|> pA <|> pT <|> pN <|> pU <|>
       pP <|> pC <|> pH

{--- Parse an integer (with leading whitespace) ---}
pInt :: Parser Int
pInt = try negInt <|> try posInt <?> "a (signed) integer"
  where
    posInt = liftM read $ spaces >> (many1 digit)
    negInt = liftM (\s -> - (read s)) $ spaces >> char '-' >> (many1 digit)

{--- Parse an pair of integer (with leading whitespace) without parens ---}
pIntP :: Parser (Int,Int)
pIntP = do
  n1 <- spaces >> (many1 digit)
  char ','
  n2 <- many1 digit
  return $ (read n1, read n2)

{--- Parse three pairs of integer (with leading whitespace) ---}
pInt3P :: Parser ((Int,Int),(Int,Int),(Int,Int))
pInt3P = do
  (n1,n2) <- pIntP
  (n3,n4) <- pIntP
  (n5,n6) <- pIntP
  return $ ((n1,n2),(n3,n4),(n5,n6))

{--- Parse attributes ---}
-- Parse a block of attributes to a list
pAtts :: Parser [Att]
pAtts = do 
  string "{\n" 
  atts <- many p1Att
  string "}\n"
  return atts 

-- Parse a single attribute
p1Att :: Parser Att
p1Att = do 
  char 'T'
  x1:y1:color:size:visibility:show_name_value:angle:alignment:
    [] <- replicateM 8 pInt
  num_lines <- try pInt <|> return 1
  newline
  key <- many (noneOf "=")
  char '='
  value <- many (noneOf "\n")
  newline
  return $
    Att x1 y1 color size visibility show_name_value angle alignment key value

{--- Parse (the) Version ---}
pv :: Parser GSchem
pv = do 
  char 'v'
  version <- pInt
  fileformat_version <- if (version >= 20031004) then pInt else return (-1)
  newline
  return $ Version version fileformat_version

{--- Parse a Line ---}
pL :: Parser GSchem
pL = do 
  char 'L'
  x1:y1:x2:y2:color:width:capstyle:dashstyle:dashlength:dashspace:
    [] <- replicateM 10 pInt
  newline
  atts <- choice [pAtts, return []]
  return $ 
    L x1 y1 x2 y2 color width capstyle dashstyle dashlength dashspace atts 

{--- Parse a Graphic ---}
pG :: Parser GSchem
pG = do 
  char 'G'
  x1:y1:width:height:angle:ratio:mirrored:embedded:[] <- replicateM 11 pInt
  newline
  filename <- many (noneOf "\n")
  newline
  enc_data <- if (embedded == 1) 
    then do { enc' <- many (noneOf "\n");
              string "\n.";
              return enc' }
    else return ""
  newline
  atts <- choice [pAtts, return []] 
  return $ 
    G x1 y1 width height angle ratio mirrored embedded filename 
      enc_data atts


{--- Parse a Box ---}
pB :: Parser GSchem
pB = do 
  char 'B'
  x1:y1:box_width:box_height:color:line_width:capstyle:dashstyle:dashlength:
    dashspace:filltype:fillwidth:angle1:pitch1:angle2:pitch2:
    [] <- replicateM 16 pInt
  newline
  atts <- choice [pAtts, return []] 
  return $ 
    B x1 y1 box_width box_height color line_width capstyle dashstyle dashlength 
      dashspace filltype fillwidth angle1 pitch1 angle2 pitch2 atts

{--- Parse a Circle ---}
pV :: Parser GSchem
pV = do 
  char 'V'
  x:y:radius:color:line_width:capstyle:dashtype:dashlength:dashspace:
    filltype:fillwidth:angle1:pitch1:angle2:pitch2:[] <- replicateM 16 pInt
  newline
  atts <- choice [pAtts, return []] 
  return $ 
    V x y radius color line_width capstyle dashtype dashlength dashspace 
      filltype fillwidth angle1 pitch1 angle2 pitch2 atts

{--- Parse an Arc ---}
pA :: Parser GSchem
pA = do
  char 'A'
  x:y:radius:startangle:sweepangle:color:line_width:capstyle:dashstyle:
    dashlength:dashspace:[] <- replicateM 11 pInt
  newline
  atts <- choice [pAtts, return []] 
  return $ 
    A x y radius startangle sweepangle color line_width capstyle dashstyle 
      dashlength dashspace atts

{--- Parse a Text Object ---}
pT :: Parser GSchem
pT = do
  char 'T'
  x:y:color:size:visibility:show_name_value:angle:alignment:
    [] <- replicateM 8 pInt
  num_lines <- try pInt <|> return 1
  newline
  text <- replicateM num_lines pToNL
  atts <- choice [pAtts, return []]
  return $ 
    T x y color size visibility show_name_value angle alignment num_lines 
      text atts
  where
    {--- Parse a string up to a New Line ---}
    pToNL = do ln <- many (noneOf "\n")
               newline
               return ln

{-- Parse a Net --}
pN :: Parser GSchem
pN = do
  char 'N'
  x1:y1:x2:y2:color:[] <- replicateM 5 pInt
  newline
  atts <- choice [pAtts, return []]
  return $ N x1 y1 x2 y2 color []

{-- Parse a Bus --}
pU :: Parser GSchem
pU = do
  char 'U'
  x1:y1:x2:y2:color:ripperdir:[] <- replicateM 6 pInt
  newline
  atts <- choice [pAtts, return []]
  return $ U x1 y1 x2 y2 color ripperdir atts

{-- Parse a Pin --}
pP :: Parser GSchem
pP = do
  char 'P'
  x1:y1:x2:y2:color:pintype:whichend:[] <- replicateM 7 pInt
  atts <- choice [pAtts, return []]
  newline
  return $ P x1 y1 x2 y2 color pintype whichend atts

{-- Parse a Component --}
-- Parse a Component Object
pC :: Parser GSchem
pC = do
  char 'C'
  x:y:selectable:angle:mirror:[] <- replicateM 5 pInt
  basename <- spaces >> many (noneOf "\n")
  newline
  subcomp <- choice [pSubComp, return Null]
  atts <- choice [pAtts, return []]
  return $ C x y selectable angle mirror basename subcomp atts

-- Parse a subcomponent
pSubComp :: Parser GSchem
pSubComp = do
  string "[\n" 
  objs <- many pObj
  string "]\n"
  -- Coerce the list into a GSchem using :+: and Null
  return $ foldr (:+:) Null objs

{-- Parse a Path --}
-- Parse a GSchem patH object
pH :: Parser GSchem
pH = do
  char 'H'
  color:width:capstyle:dashstyle:dashlength:dashspace:filltype:fillwidth:
    angle1:pitch1:angle2:pitch2:num_lines:[] <- replicateM 13 pInt
  newline
  path <- replicateM num_lines pPath
  atts <- choice [pAtts, return []]
  return $ H color width capstyle dashstyle dashlength dashspace filltype 
             fillwidth angle1 pitch1 angle2 pitch2 num_lines path atts

-- Parse a path object
pPath :: Parser Path
pPath = pMM <|> pMm <|> pLL <|> pLl <|> pCC <|> pCc <|> pZ

-- Parse an (absolute) MoveTo instruction
pMM :: Parser Path
pMM = do
  char 'M'
  moves <- many pIntP
  newline
  return $ MM moves

-- Parse a (relative) MoveTo instruction
pMm :: Parser Path
pMm = do
  char 'm'
  moves <- many pIntP
  newline
  return $ Mm moves

-- Parse an (absolute) Line instruction
pLL :: Parser Path
pLL = do
  char 'L'
  moves <- many pIntP
  newline
  return $ LL moves

-- Parse a (relative) Line instruction
pLl :: Parser Path
pLl = do
  char 'l'
  moves <- many pIntP
  newline
  return $ Ll moves

-- Parse an (absolute) Bezier Curve
pCC :: Parser Path
pCC = do
  char 'C'
  moves <- many pInt3P 
  newline
  return $ CC moves

-- Parse an (relative) Bezier Curve
pCc :: Parser Path
pCc = do
  char 'c'
  moves <- many pInt3P 
  newline
  return $ Cc moves

-- Parse an Close-path instruction
pZ :: Parser Path
pZ = do
   char 'Z' <|> char 'z'
   newline
   return Z
