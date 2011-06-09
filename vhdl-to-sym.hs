{-# OPTIONS_GHC -XRecordWildCards #-}

module Main where

import System (getArgs,getProgName)
import System.Exit (exitSuccess)
import System.IO
import Control.Monad (forM_, liftM)
import Geda.Core
import Geda.IO
import Language.VHDL.Parser (entityStrings, readEntity)
import Language.VHDL.Core

-- Dimensional parameters for pins
len :: Int
len = 300

spacing :: Int
spacing = 400

-- Draw a single pin
dpin whichend x y port = let
  (ident, dir, typ, val) = port
  (x1,y1,x2,y2) = case dir of 
    IN -> (x,y,x+len,y)
    OUT -> (x+len,y,x,y)
    INOUT -> (x,y,x,y+len)
  color = 1
  pintype = 0
  atts = []
  in P {..}

-- Draw several horizontal pins parallel to one another, 
-- forming a verticle line
vlpins whichend hoffset voffset vheight ports = 
  let voffsetp = 
        voffset + (max 0 $ round $ toRational 
                   (vheight - length ports * spacing) / 2)
  in zipWith (dpin whichend hoffset) 
             [voffsetp + i*spacing | i <- [1..length ports]]
             ports
     
-- Draw several verticle pins parallel to one another, 
-- forming a horizontal line
hlpins whichend hoffset voffset hwidth ports = 
  let hoffsetp = 
        hoffset + (max 0 $ round $ toRational
                   (hwidth - length ports * spacing) / 2)
  in zipWith (\x -> dpin whichend x voffset)
             [hoffsetp + i*spacing | i <- [1..length ports]]
             ports

--entity2schematic :: Entity -> [GSchem]
entity2schematic out_dir entity = 
  [ Basename (identifier entity),
    Dirname (out_dir),
    Version 20110115 2 ] ++
  (vlpins 0 0 0 vheight inpins) ++
  (vlpins 0 (hoffset + hwidth)  0 vheight outpins) ++
  (hlpins 0 hoffset vheight hwidth iopins)
  where
    inpins = filter (\(_,dir,_,_) -> dir == IN) $ port entity
    outpins = filter (\(_,dir,_,_) -> dir == OUT) $ port entity
    iopins = filter (\(_,dir,_,_) -> dir == INOUT) $ port entity
    hoffset = len
    vheight = spacing * max (length inpins) (length outpins)
    hwidth = spacing * length iopins

-- Process an entity to a schematic in the IO monad
proc out_dir in_fns = do
  in_fhs <- if (in_fns == ["-"] || in_fns == []) 
    then return [stdin]
    else mapM (`openFile` ReadMode) in_fns
  cnts <- mapM hGetContents in_fhs
  let ents =  
             map ((\(Right x) -> x) . readEntity) $ 
             concat $ map entityStrings cnts
  --mapM_ (fnPutGSchematic "-") $
    mapM_ (putStr.show) $
         map (entity2schematic out_dir) ents
  if (in_fns == ["-"] || in_fns == []) 
    then return () 
    else mapM_ hClose in_fhs

main :: IO ()
main = do
  -- Parse command line arguments to determine input/output behavior
  pn <- getProgName
  args <- getArgs
  if (any (=="--help") args || args == []) then
    putStrLn ("Usage: " ++ pn ++ "[OUTPUTDIRECTORY] [[INPUT1] [INPUT2] [INPUT3] ...]") >> exitSuccess
    else return ()
  proc (head args) (tail args)
  exitSuccess