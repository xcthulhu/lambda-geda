{-# OPTIONS_GHC -XRecordWildCards #-}

module Main where

import System (getArgs,getProgName)
import System.Exit (exitSuccess)
import System.IO
import Control.Monad (join, forM_, liftM)
import Char (toLower)
import Data.List ( isInfixOf, 
                   isPrefixOf,
                   sortBy,
                   intercalate,
                   find )
import Geda.Core
import Geda.IO
import Language.VHDL.Parser ( readArchs,
                              readEntities, 
                              readLibs,
                              readUses )
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
showDir INOUT = "inout"

-- Helper function for determining if something is a clock
itsAClock (x, _, _, _) = "clk" `isInfixOf` (map toLower x)

-- sqrt 2
sqrt2 = 1.4142135623730951

-- Draws an arrowhead by rotating (x2,y2) around (x1,y1) by +- 3*pi/4
-- then recenters at (xp,yp)
-- finally scales by sz
-- Draws lines from (xp,yp) to the results
arrowHead :: (Int,Int) -> Rational -> (Int,Int) -> (Int,Int) -> [GSchem]
arrowHead (px,py) sz (x1,y1) (x2,y2) = let
  -- Prototype Line for expediency
  arr = L { x1 = px, y1 = py, x2 = 0, y2 = 0, 
            color=1, line_width = 0, capstyle = 0, 
            dashstyle = 0, dashlength = -1, 
            dashspace = -1, atts = [] }
  in 
     -- Use incomprehensible closed forms representing our geometrical operations
     -- Exercise for reader: write this thingy using rotation matrices in Mathematica and verify this nonsense
     [ arr{ x2 = px + (round $ sz / sqrt2 * toRational (x1 - x2 + y1 - y2)), 
            y2 = py + (round $ sz / sqrt2 * toRational (-x1 + x2 + y1 - y2))}
     , arr{ x2 = px + (round $ sz / sqrt2 * toRational (x1 - x2 - y1 + y2)), 
            y2 = py + (round $ sz / sqrt2 * toRational (x1 - x2 + y1 - y2))}
     ]

-- Finds the height of a gschem
-- Break objects in list don't have y1 and y2 methods
gschemHeight :: [GSchem] -> Int
gschemHeight gs = foldl max (-1000000) $ (map y1 gs) ++ (map y2 gs) 

-- Draw a single pin
-- This is really big - refactor?
dpin :: (Int,Int) -> (Int,Int) -> Port -> [GSchem]
dpin p1@(x1,y1) p2@(x2,y2) port = let
  (ident, dir, typ, val) = port
  color = 1
  sz = 1/6
  -- Draw arrows
  arrow_head = case dir of
    IN -> arrowHead p1 sz p1 p2
    OUT -> arrowHead p1 sz p2 p1
    INOUT -> arrowHead p2 sz p1 p2

  -- Pin attribute prototype
  att = Att {x1=x2, y1=y2, color=5, size=text_size - 6,
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
  path = [ MM [ (x2 + (round $ toRational (x2-x1) * sz),
                 y2 + (round $ toRational (y2-y1) * sz))]
         , LL [ (x2 + (round $ toRational (y1-y2) * sz),
                 y2 + (round $ toRational (x2-x1) * sz)) 
              , (x2 + (round $ toRational (y2-y1) * sz),
                 y2 + (round $ toRational (x1-x2) * sz))]
         , Z ]
  in [ P { pintype=0, whichend = 0, 
           atts=[ att{ key="pinnumber", value=ident }
                , att{ y1=y2+text_size*7,
                       key="port_mode", value=(showDir dir) }
                , att{ y1=y2+2*text_size*7,
                       key="port_type", value=typ }
                , att{ x1 = round $ 
                            toRational (x1+5*x2+y1-y2) * sz,
                       y1 = 10 + (gschemHeight arrow_head),
                       alignment=(if x1 < x2 then 6 else 0),
                       visibility = 1, show_name_value = 1,
                       key="pinlabel", value=ident }
                ], .. }
     ] ++ arrow_head ++ clk

-- Calculates the offset from input parameters
offp offset d n = 
  offset + (max 0 $ round $ toRational 
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
  in join $ zipWith (uncurry dpin) coords ports
     
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
  in join $ zipWith (uncurry dpin) coords ports

-- Calculates the width from an input string
calcWidth s = (length s) * 10 * text_size + len + (length "U?") * 10 * text_size

-- Draw a box with component name in the center
mkbox x y hwidth vheight entity =
   [ H      { color = 3, line_width = 0, capstyle = 0, 
              dashstyle = 0, dashlength = -1, dashspace = -1, 
              filltype = 0, fillwidth = -1, angle1 = -1, 
              pitch1 = -1, angle2 = -1, pitch2 = -1, 
              num_lines = length path, .. }
   , Att    { x1 = x + width, y1 = y + vheight + 20, color = 8, 
              visibility = 1, show_name_value = 1, 
              alignment = 6, num_lines = 1, 
              key="refdes", value="U?", .. }
   , invatt { x1 = x, y1 = y + vheight, 
              key="device", value=devname }
   , T      { x1 = round (toRational (2*x + width) / 2), 
              y1 = round (toRational (2*y + vheight) / 2), 
              color = 9, visibility = 1, show_name_value = 0, 
              alignment = 4, num_lines=1,
              text = [devname], .. } ]
   ++ case (architecture entity) of
       Nothing -> []
       Just a -> [ invatt { x1 = x + width + 2*len, y1 = 0, 
                            key="architecture", value=a } ]
   ++ case (workdir entity) of
       Nothing -> []
       Just work -> [ invatt { x1 = x + width + 2*len, 
                               y1 = len, 
                               key="workdir", 
                               value=work } ]
   ++ case (libraries entity) of
       [] -> []
       libs -> [ invatt { x1 = x + width + 2*len, 
                          y1 = 2*len, 
                          key="libraries", 
                          value=intercalate " " libs } ]
   ++ case (uses entity) of
       [] -> []
       us -> [ invatt { x1 = x + width + 2*len, 
                        y1 = 3*len, 
                        key="uses", 
                        value=intercalate " " us } ]
   ++ write_gens (generic entity)
     where
       devname = (identifier entity)
       size = text_size
       width = max hwidth (calcWidth devname)
       path = [ MM [(x,y)]
              , Ll [ (0, vheight) 
                   , (width, 0)
                   , (0,-vheight) ]
              , Z ]
       angle = 0
       atts = []
       invatt = Att { x1 = 0, y1 = 0, color = 5, 
                      visibility = 0, show_name_value = 0, 
                      alignment = 0, num_lines = 1, 
                      key="", value="", .. }
       visatt = invatt { visibility = 1 }
       write_gens gens = zipWith 
                         (\(ident,typ,val) n -> 
                           visatt { x1 = x, y1 = y + n*len, 
                                    key = filter (/='\n') $ ident ++ " : " ++ typ,
                                    value = filter (/='\n') $ "?" ++ maybe "" id val })
                         gens [0..(length gens)-1]

entity2schematic :: FilePath -> Entity -> [GSchem]
entity2schematic out_dir entity = 
  [ Basename 
    (identifier entity 
     ++ maybe "" (\x -> "_" ++ x) (architecture entity)
     ++".sym")
  , Dirname out_dir
  , Version 20110115 2 ]
  ++ mkbox hoffset voffset hwidth vheight entity
  ++ vlpins 0 (hoffset - len)    voffset vheight inpins
  ++ vlpins 1 (hoffset + hwidth) voffset vheight outpins
  where
    its_a_clock x _ = if (itsAClock x) then GT else LT 
    inpins = sortBy its_a_clock $
             [(ident, dir, typ, val) | 
              (ident, dir, typ, val) <- port entity, 
              dir == IN, not ("wb" `isPrefixOf` ident) ]
    outpins = [ (ident, dir, typ, val) | 
                (ident, dir, typ, val) <- port entity, 
                ("wb" `isPrefixOf` ident) ] 
              ++
              [ (ident, dir, typ, val) | 
                (ident, dir, typ, val) <- port entity, 
                dir == INOUT, 
                not ("wb" `isPrefixOf` ident) ] 
              ++
              [ (ident, dir, typ, val) | 
                (ident, dir, typ, val) <- port entity, 
                dir == OUT, not ("wb" `isPrefixOf` ident) ]
    voffset = len
    hoffset = 2*len
    hwidth = calcWidth $ identifier entity
    vheight = max hwidth $ 
                  spacing * (1 + max (length inpins) 
                                     (length outpins))

-- Converts parsec error coproduct type to Maybe monad
toMaybe (Left _) = Nothing
toMaybe (Right a) = Just a

-- Similar to sequence for Maybe, only throws away failures
cleanSequence = sequence . (filter success)
  where
    success (Just _) = True
    success Nothing = False

-- A clean mapM for Maybe
cleanMapM f ls = cleanSequence $ map f ls

-- Unjustly exits the Maybe monad
unJust (Just a) = a

-- Probably should make use of Maybe monad transformer
maybeMkEnts workdir vhdlcode = do 
  e <- (unJust . toMaybe . readEntities) vhdlcode
  return $ do { libraries <- (toMaybe . readLibs) vhdlcode
              ; uses <- (toMaybe . readUses) vhdlcode
              ; return e { workdir = workdir,
                           libraries = libraries,
                           uses = uses } }

-- Given an assortment of file contents, this
-- creates all of the entities described
mkAllEnts workdir cnts = do
  let ents = join $ unJust $ cleanMapM (cleanSequence . 
                                        (maybeMkEnts 
                                         workdir))
                                       cnts
  e <- ents
  let idt = identifier e
  let archs = (unJust . toMaybe . (readArchs idt)) (join cnts)
  fulle <- if (archs /= [])
           then [ e { architecture = Just a } | a <- archs ]
           else [ e ]
  return fulle

-- Process an entity to a schematic in the IO monad
proc out_dir in_fns workdir = do
  in_fhs <- if (in_fns == ["-"] || in_fns == []) 
    then return [stdin]
    else mapM (`openFile` ReadMode) in_fns
  cnts <- mapM hGetContents in_fhs
  let ents = mkAllEnts workdir cnts
  mapM_ (fnPutGSchematic "_") $
         map (entity2schematic out_dir) ents
  if (in_fns == ["-"] || in_fns == [])
    then return () 
    else mapM_ hClose in_fhs

main :: IO ()
main = do
  -- Parse command line arguments to determine input/output behavior
  pn <- getProgName
  raw_args <- getArgs
  let workdir = maybe "work" (tail.(dropWhile (/='=')))
                $ find ("--workdir=" `isPrefixOf`) raw_args
  let args = [ a | a <- raw_args, 
                   not ("--workdir=" `isPrefixOf` a) ]
  if (any (=="--help") args || args == []) 
    then ( putStrLn $ "Usage: " ++ pn 
           ++ " [OUTPUTDIRECTORY]" 
           ++ " [[INPUT1] [INPUT2] [INPUT3] ...]" )
      >> exitSuccess
    else return ()
  proc (head args) (tail args) $ Just workdir
  exitSuccess
