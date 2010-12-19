module Main( main ) where

import System(getArgs,getProgName)
import System.Exit
import System.FilePath ((</>))
import System.IO
import Geda.Hierarchical

main = do
  pn <- getProgName
  args <- getArgs
  if (any (=="--help") args) 
    then putStrLn ("Usage: " ++ pn ++ 
                   " [OUTPUTDIRECTORY] [[INPUT1] [INPUT2] [INPUT3]]") 
         >> exitSuccess
    else return ()
  let outdir = head args          
  let files = tail args
  raw_schems <- mapM getGSchematic files
  (comps, sources) <- getLibraries
  hierarchical_schems <- expandHierarchies comps sources raw_schems
  let flattened_schems = flattenHierarchies hierarchical_schems
  sequence_ $ do { schem <- flattened_schems
                 ; let bname = baseName schem
                 ; return $ putGSchematic (outdir</>bname) schem }