module Main( main ) where

import System(getArgs,getProgName)
import System.Exit
import System.FilePath ((</>))
import System.IO
import Geda.Hierarchical
import Geda.IO

main = do
  pn <- getProgName
  args <- getArgs
  if ((any (=="--help") args) || args == [])
    then putStrLn ("Usage: " ++ pn ++ 
                   " [OUTPUTDIRECTORY] [[INPUT1] [INPUT2] [INPUT3]]") 
         >> exitSuccess
    else return ()
  let outdir = head args          
  let files = tail args
  raw_schems <- mapM getGSchematic files
  (comps, sources) <- getLibraries
  schems' <- expandHierarchies comps sources raw_schems
  let fixed_schems =   flattenHierarchies
                     $ unembedHierarchies schems'  
  sequence_ $ do { schem <- fixed_schems
                 ; let bname = baseName schem
                 ; return $ putGSchematic (outdir</>bname) schem }
