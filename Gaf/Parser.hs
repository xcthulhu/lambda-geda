module Gaf.Parser (readGSchem) where

import Text.ParserCombinators.Parsec hiding (spaces,newline)
import Control.Monad (join,replicateM)
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
pGSchem :: Parser [GSchem]
pGSchem = do
  obs <- many pObj
  eof
  return obs

readGSchem :: String -> Either ParseError [GSchem]
readGSchem = parse pGSchem "gschem"

{--- Parse Any Object ---}
-- This object parses an entry in a GSchem file to a corresponding
-- object of type GSchem
pObj :: Parser GSchem
pObj = pv <|> pL <|> pG <|> pB <|> pV <|> pA <|> try pF <|> pT <|> pN <|> pU <|>
       pP <|> pC <|> pH

{--- Parse a newline or eof ---}
newline :: Parser ()
newline = try (char '\n' >> return ()) <|> eof <?> "newline or EOF"

{--- Parse some whitespace ---}
spaces :: Parser ()
spaces = many (char ' ') >> return ()

{--- Parse an integer (with trailing whitespace) ---}
pInt :: Parser Int
pInt = try negInt <|> try posInt <?> "(signed) integer"
  where
    posInt = do { ds <- many1 digit ; spaces ; return $ read ds }
    negInt = do { char '-' ; ds <- many1 digit ; spaces ; return $ - (read ds) }

{--- Parse an pair of integer (with leading whitespace) without parens ---}
pIntP :: Parser (Int,Int)
pIntP = do
  n1 <- pInt
  char ','
  n2 <- pInt
  return $ (n1,n2)

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
  string "}"
  newline
  return atts 

-- Parse a single attribute
p1Att :: Parser Att
p1Att = do 
  char 'T'
  spaces
  x1:y1:color:size:visibility:show_name_value:angle:alignment:
    [] <- replicateM 8 pInt
  num_lines <- try pInt <|> return 1
  newline
  key <- many (noneOf "=\n")
  char '='
  values <- replicateM num_lines pLine
  let value = join values
  return $
    Att x1 y1 color size visibility show_name_value angle alignment num_lines
        key value
  where
    pLine = do val <- many (noneOf "\n")
               newline
               return val

{--- Parse (the) Version ---}
pv :: Parser GSchem
pv = do 
  char 'v'
  spaces
  version <- pInt
  fileformat_version <- if (version >= 20031004) then pInt else return (-1)
  newline
  return $ Version version fileformat_version

-- FIXME! Lots of combinators look like this; maybe use TemplateHaskell?
{--- Parse a Line ---}
pL :: Parser GSchem
pL = do 
  char 'L'
  spaces
  x1:y1:x2:y2:color:width:capstyle:dashstyle:dashlength:dashspace:
    [] <- replicateM 10 pInt
  newline
  atts <- try pAtts <|> return []
  return $ 
    L x1 y1 x2 y2 color width capstyle dashstyle dashlength dashspace atts 

{--- Parse a Graphic ---}
pG :: Parser GSchem
pG = do 
  char 'G'
  spaces
  x1:y1:width:height:angle:ratio:mirrored:embedded:[] <- replicateM 11 pInt
  newline
  filename <- many (noneOf "\n")
  newline
  enc_data <- if (embedded == 1) 
    then do { enc' <- many (noneOf "\n");
              string "\n.\n";
              return enc' }
    else return ""
  atts <- try pAtts <|> return [] 
  return $ 
    G x1 y1 width height angle ratio mirrored embedded filename 
      enc_data atts


{--- Parse a Box ---}
pB :: Parser GSchem
pB = do 
  char 'B'
  spaces
  x1:y1:box_width:box_height:color:line_width:capstyle:dashstyle:dashlength:
    dashspace:filltype:fillwidth:angle1:pitch1:angle2:pitch2:
    [] <- replicateM 16 pInt
  newline
  atts <- try pAtts <|> return [] 
  return $ 
    B x1 y1 box_width box_height color line_width capstyle dashstyle dashlength 
      dashspace filltype fillwidth angle1 pitch1 angle2 pitch2 atts

{--- Parse a Circle ---}
pV :: Parser GSchem
pV = do 
  char 'V'
  spaces
  x:y:radius:color:line_width:capstyle:dashtype:dashlength:dashspace:
    filltype:fillwidth:angle1:pitch1:angle2:pitch2:[] <- replicateM 15 pInt
  newline
  atts <- try pAtts <|> return [] 
  return $ 
    V x y radius color line_width capstyle dashtype dashlength dashspace 
      filltype fillwidth angle1 pitch1 angle2 pitch2 atts

{--- Parse an Arc ---}
pA :: Parser GSchem
pA = do
  char 'A'
  spaces
  x:y:radius:startangle:sweepangle:color:line_width:capstyle:dashstyle:
    dashlength:dashspace:[] <- replicateM 11 pInt
  newline
  atts <- try pAtts <|> return [] 
  return $ 
    A x y radius startangle sweepangle color line_width capstyle dashstyle 
      dashlength dashspace atts

{--- Parse a Text Object ---}
pT :: Parser GSchem
pT = do
  char 'T'
  spaces
  x:y:color:size:visibility:show_name_value:angle:alignment:
    [] <- replicateM 8 pInt
  -- Older schematics don't have line number counts
  num_lines <- try pInt <|> return 1
  newline
  text <- replicateM num_lines pToNL
  atts <- try pAtts <|> return []
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
  spaces
  x1:y1:x2:y2:color:[] <- replicateM 5 pInt
  newline
  atts <- try pAtts <|> return []
  return $ N x1 y1 x2 y2 color []

{-- Parse a Bus --}
pU :: Parser GSchem
pU = do
  char 'U'
  spaces
  x1:y1:x2:y2:color:ripperdir:[] <- replicateM 6 pInt
  newline
  atts <- try pAtts <|> return []
  return $ U x1 y1 x2 y2 color ripperdir atts

{-- Parse a Pin --}
pP :: Parser GSchem
pP = do
  char 'P'
  spaces
  x1:y1:x2:y2:color:[] <- replicateM 5 pInt
  -- Some schematics don't have a pintype, and whichend is calculated sometimes 
  pintype <- try pInt <|> return (-1)
  whichend <- try pInt <|> return (-1)
  newline
  atts <- try pAtts <|> return []
  return $ P x1 y1 x2 y2 color pintype whichend atts

{-- Parse a Component --}
-- Parse a Component Object
pC :: Parser GSchem
pC = do
  char 'C'
  spaces
  x:y:selectable:angle:mirror:[] <- replicateM 5 pInt
  basename <- many (noneOf "\n")
  newline
  subcomp <- try pSubComp <|> return []
  atts <- try pAtts <|> return []
  return $ C x y selectable angle mirror basename subcomp atts

-- Parse a subcomponent
pSubComp :: Parser [GSchem]
pSubComp = do
  string "[\n" 
  objs <- many pObj
  char ']'
  newline
  return $ objs

{-- Parse a Path --}
-- Parse a GSchem patH object
pH :: Parser GSchem
pH = do
  char 'H'
  spaces
  color:width:capstyle:dashstyle:dashlength:dashspace:filltype:fillwidth:
    angle1:pitch1:angle2:pitch2:num_lines:[] <- replicateM 13 pInt
  newline
  path <- replicateM num_lines pPath
  atts <- try pAtts <|> return []
  return $ H color width capstyle dashstyle dashlength dashspace filltype 
             fillwidth angle1 pitch1 angle2 pitch2 num_lines path atts

-- Parse a path object
pPath :: Parser Path
pPath = pMM <|> pMm <|> pLL <|> pLl <|> pCC <|> pCc <|> pZ

-- Parse an (absolute) MoveTo instruction
pMM :: Parser Path
pMM = do
  char 'M'
  spaces
  moves <- many pIntP
  newline
  return $ MM moves

-- Parse a (relative) MoveTo instruction
pMm :: Parser Path
pMm = do
  char 'm'
  spaces
  moves <- many pIntP
  newline
  return $ Mm moves

-- Parse an (absolute) Line instruction
pLL :: Parser Path
pLL = do
  char 'L'
  spaces
  moves <- many pIntP
  newline
  return $ LL moves

-- Parse a (relative) Line instruction
pLl :: Parser Path
pLl = do
  char 'l'
  spaces
  moves <- many pIntP
  newline
  return $ Ll moves

-- Parse an (absolute) Bezier Curve
pCC :: Parser Path
pCC = do
  char 'C'
  spaces
  moves <- many pInt3P 
  newline
  return $ CC moves

-- Parse an (relative) Bezier Curve
pCc :: Parser Path
pCc = do
  char 'c'
  spaces
  moves <- many pInt3P 
  newline
  return $ Cc moves

-- Parse an Close-path instruction
pZ :: Parser Path
pZ = do
   char 'Z' <|> char 'z'
   spaces
   return Z

{-- Parse a Floating Attribute --}
pF = do
   att <- p1Att
   return $ F att
