{-# OPTIONS_GHC -XRecordWildCards #-}

module Main where

import System (getArgs,getProgName)
import System.Exit (exitSuccess)
import System.IO
import Control.Monad (forM_, liftM)
import Char (toLower)
import Data.List (isInfixOf, sortBy)
import Geda.Core
import Geda.IO
import Language.VHDL.Parser (entityStrings, readEntity)
import Language.VHDL.Core

-- Dimensional parameters for pins
len :: Int
len = 300

spacing :: Int
spacing = 400

-- Size for text
text_size :: Int
text_size = 10

-- Convert the pin direction for GSchem's pintype
showDir IN = "in"
showDir OUT = "out"
showDir INOUT = "io"

-- Helper function for extractling line heights
maxLineHeight ln1 ln2 = max (y2 ln1) (y2 ln2)

-- Helper function for determining if something is a clock
itsAClock (x, _, _, _) = "clk" `isInfixOf` (map toLower x)

-- Draw a single pin
dpin :: ((Int,Int),(Int,Int)) -> Port -> [GSchem]
dpin ((x1,y1),(x2,y2)) port = let
  (ident, dir, typ, val) = port
  color = 1
  -- Arrow prototype for expediency
  arr = L { line_width = 0, capstyle = 0, dashstyle = 0,
            dashlength = -1, dashspace = -1, atts = [], 
            .. }
  -- Draw arrows
  (arrowp1,arrowp2) = case dir of
    IN -> 
      (arr{x2=round $ toRational (7*x1-x2+y1-y2) / 6,
           y2=round $ toRational (-x1+x2+7*y1-y2) / 6},
       arr{x2=round $ toRational (7*x1-x2-y1+y2) / 6,
           y2=round $ toRational (x1-x2+7*y1-y2) / 6})
    OUT -> 
      (arr{x2=round $ toRational (5*x1+x2+y1-y2) / 6,
           y2=round $ toRational (-x1+x2+5*y1+y2) / 6},
       arr{x2=round $ toRational (5*x1+x2-y1+y2) / 6,
           y2=round $ toRational (x1-x2+5*y1+y2) / 6})
    INOUT -> 
      (arr{x1=x2, y1=y2,
           x2=round $ toRational (x1+5*x2+y1-y2) / 6,
           y2=round $ toRational (-x1+x2+y1+5*y2)/ 6},
       arr{x1=x2, y1=y2,
           x2=round $ toRational (x1+5*x2-y1+y2) / 6,
           y2=round $ toRational (x1-x2+y1+5*y2) / 6})
  -- Pin attribute prototype
  att = Att {x1=x2, y1=y2, color=5, size=text_size - 5,
             visibility=0, show_name_value=0, angle=0,
             alignment=(if x1 < x2 then 1 else 7),
             num_lines=1, key="", value="", atts=[]}
  -- Draw a little clock triangle at (x2,y2) 
  -- ...if the port is a clock
  clk = if (not $ (itsAClock port) && (dir == IN)) 
        then [] 
        else [ H { color = 3, line_width = 0, capstyle = 0, 
                   dashstyle = 0, dashlength = -1, 
                   dashspace = -1, filltype = 0, 
                   fillwidth = -1, angle1 = -1, 
                   pitch1 = -1, angle2 = -1, pitch2 = -1, 
                   num_lines = length path, atts=[], .. } ]
  path = [ MM [ (x2 + (round $ toRational (x2-x1) / 6),
                 y2 + (round $ toRational (y2-y1) / 6))]
         , LL [ (x2 + (round $ toRational (y1-y2) / 6),
                 y2 + (round $ toRational (x2-x1) / 6)) 
              , (x2 + (round $ toRational (y2-y1) / 6),
                 y2 + (round $ toRational (x1-x2) / 6))]
         , Z ]
  in [ P {pintype=0, whichend = 0, 
          atts=[ att{ key="pinnumber", value=ident }
               , att{ y1=y2+text_size*10,
                      key="pinseq", value=ident }
               , att{ y1=y2+2*text_size*10,
                      key="pintype", value=(showDir dir) }
               , att{ x1 = round $ 
                           toRational (x1+5*x2+y1-y2) / 6,
                      y1 = maxLineHeight arrowp1 arrowp2,
                      alignment=(if x1 < x2 then 6 else 0),
                      visibility = 1, show_name_value = 1,
                      key="pinlabel", value=ident }
               ], ..}
     , arrowp1
     , arrowp2 ] ++ clk

-- Calculates the offset from input parameters
offp offset d n = offset + (max 0 $ round $ toRational
                         (d - ((n+1) * spacing)) / 2)

-- Draw several horizontal pins parallel to one another, 
-- forming a verticle line
vlpins whichend hoffset voffset vheight ports = 
  let voffsetp = offp voffset vheight (length ports)
      (x1,x2) = if whichend == 0 
                 then (hoffset, hoffset + len)
                 else (hoffset + len, hoffset)
      coords = do i <- [1..length ports]
                  let y = voffsetp + i*spacing
                  return ((x1,y),(x2,y))
  in concat $ zipWith dpin coords ports
     
-- Draw several verticle pins parallel to one another, 
-- forming a horizontal line
hlpins whichend hoffset voffset hwidth ports = 
  let hoffsetp = offp hoffset hwidth (length ports)
      (y1,y2) = if whichend == 0 
                 then (voffset, voffset + len)
                 else (voffset + len, voffset)
      coords = do i <- [1..length ports]
                  let x = hoffsetp + i*spacing
                  return ((x,y1),(x,y2))
  in concat $ zipWith dpin coords ports

-- Calculates the width from an input string
calcWidth s = (length s) * 10 * text_size + len + (length "U?") * 10 * text_size

-- Draw a box with component name in the center
mkbox x y hwidth vheight devname =
  let size = text_size
      width = max hwidth (calcWidth devname)
      path = [ MM [(x,y)]
             , Ll [ (0, vheight) 
                  , (width, 0)
                  , (0,-vheight) ]
             , Z ]
      angle = 0
      atts = [] in
  [ H   { color = 3, line_width = 0, capstyle = 0, 
          dashstyle = 0, dashlength = -1, dashspace = -1, 
          filltype = 0, fillwidth = -1, angle1 = -1, 
          pitch1 = -1, angle2 = -1, pitch2 = -1, 
          num_lines = length path, .. }
  , Att { x1 = x + width, y1 = y + vheight + 20, color = 8, 
          visibility = 1, show_name_value = 1, 
          alignment = 6, num_lines = 1, 
          key="refdes", value="U?", .. }
  , Att { x1 = x, y1 = y + vheight, color = 5, 
          visibility = 0, show_name_value = 0, 
          alignment = 0, num_lines = 1, 
          key="device", value=devname, .. }
  , T   { x1 = round (toRational (2*x + width) / 2), 
          y1 = round (toRational (2*y + vheight) / 2), 
          color = 9, visibility = 1, show_name_value = 0, 
          alignment = 4, num_lines = 1, 
          text = [devname], .. } ] 

entity2schematic :: FilePath -> Entity -> [GSchem]
entity2schematic out_dir entity = 
  [ Basename (identifier entity ++ ".sym")
  , Dirname out_dir
  , Version 20110115 2 ]
  ++ mkbox hoffset voffset hwidth vheight (identifier entity)
  ++ vlpins 0 (hoffset - len)    voffset vheight inpins
  ++ vlpins 1 (hoffset + hwidth) voffset vheight outpins
  where
    its_a_clock x _ = if (itsAClock x) then GT else LT 
    inpins = sortBy its_a_clock $
             [(ident, dir, typ, val) | 
              (ident, dir, typ, val) <- port entity, 
              dir == IN, not ("wb" `isInfixOf` ident) ]
    outpins = [(ident, dir, typ, val) | 
              (ident, dir, typ, val) <- port entity, 
              ("wb" `isInfixOf` ident) ] ++
              [(ident, dir, typ, val) | 
               (ident, dir, typ, val) <- port entity, 
               dir == INOUT, not ("wb" `isInfixOf` ident) ] ++
              [(ident, dir, typ, val) | 
               (ident, dir, typ, val) <- port entity, 
               dir == OUT, not ("wb" `isInfixOf` ident) ]
    voffset = len
    hoffset = 2*len
    hwidth = calcWidth $ identifier entity
    vheight = max hwidth $ 
                  spacing * (1 + max (length inpins) 
                                     (length outpins))

-- Process an entity to a schematic in the IO monad
proc out_dir in_fns = do
  in_fhs <- if (in_fns == ["-"] || in_fns == []) 
    then return [stdin]
    else mapM (`openFile` ReadMode) in_fns
  cnts <- mapM hGetContents in_fhs
  let ents = map ((\(Right x) -> x) . readEntity) $ 
             concat $ map entityStrings cnts
  mapM_ (fnPutGSchematic "_") $
         map (entity2schematic out_dir) ents
  if (in_fns == ["-"] || in_fns == []) 
    then return () 
    else mapM_ hClose in_fhs

-- Debug - DO NOT USE
proc2 out_dir in_fns = do
  in_fhs <- if (in_fns == ["-"] || in_fns == []) 
    then return [stdin]
    else mapM (`openFile` ReadMode) in_fns
  cnts <- mapM hGetContents in_fhs
  mapM_ putStr $ concat $ map entityStrings cnts
  return $ map readEntity $ concat $ map entityStrings cnts
  {-
  let ents = map ((\(Right x) -> x) . readEntity) $ 
             concat $ map entityStrings cnts
  mapM_ (fnPutGSchematic "_") $
         map (entity2schematic out_dir) ents
  if (in_fns == ["-"] || in_fns == []) 
    then return () 
    else mapM_ hClose in_fhs
  -}

main :: IO ()
main = do
  -- Parse command line arguments to determine input/output behavior
  pn <- getProgName
  args <- getArgs
  if (any (=="--help") args || args == []) 
    then putStrLn ("Usage: " ++ pn ++ 
                   "[OUTPUTDIRECTORY] [[INPUT1] [INPUT2] [INPUT3] ...]") 
      >> exitSuccess
    else return ()
  proc (head args) (tail args)
  exitSuccess
