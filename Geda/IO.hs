{-# OPTIONS_GHC -XRecordWildCards #-}
module Geda.IO where

import Control.Exception
import Control.Monad (liftM2, mapM, forM)
import Data.List (lines, nubBy)
import Geda.Core
import Geda.Parser (readGSchem)
import Geda.ShowGSchem (showGSchem)
import Geda.Hierarchical
import System.Directory (canonicalizePath,getCurrentDirectory)
import System.FilePath (splitFileName, (</>), equalFilePath)
import System.IO
import System.Posix.Files (fileExist)
import System.Process (readProcess)

-- |Open and parse a schematic from a FilePath
getGSchematic :: FilePath -> IO [GSchem]
getGSchematic fn = do
  handel <- openFile fn ReadMode
  contents <- hGetContents handel
  sch <- tryParse fn contents
  hClose handel
  return sch
  where
    tryParse fn x = case readGSchem x of
      Left err -> throwIO $ ErrorCall 
                  $ "Parse of " ++ fn ++ " FAILED!\nReason: " ++ show err
      -- Prepend the Filename for reference purposes
      Right sch -> do
        let (path,base) = splitFileName fn
        return $ (Basename base):(Pathname path):sch

-- |Print a schematic to file
putGSchematic :: FilePath -> [GSchem] -> IO ()
putGSchematic fn gschem = do
  fh <- openFile fn WriteMode
  hPutStr fh $ showGSchem gschem
  hClose fh

-- |Returns the component and source libraries loaded by calling gnetlist
getLibraries :: IO ([FilePath],[FilePath]) 
getLibraries = do
  pwd <- getCurrentDirectory
  raw_list <- readProcess 
              "gnetlist" ["-q"              -- Be quiet
                         ,"-l","/dev/stdin" -- Run a command we feed
                         ,"-g","geda"       -- Pretend to netlist geda
                         ,"-o","/dev/null"  -- But throw away output
                         ,"/dev/null"       -- Don't use any input files
                         ] $ schemeProg pwd
  let entries = reverse . lines $ raw_list
  components <- mapM (canonicalizePath.tail) 
                $ filter (\x -> (x !! 0 == 'c')) entries
  sources <- mapM (canonicalizePath . tail) 
             $ filter (\x -> (x !! 0 == 's')) entries
  -- Return the lists with duplicates thrown away
  return (nubBy equalFilePath components,
          nubBy equalFilePath sources)
  where
    {- This is the scheme program we are handing gnetlist to run (using 
       stdin). It loads all of the libraries, and prints them to 
       "/dev/stdout", one library per line.  The sort of library is
       identified by the first letter. -}
    schemeProg pwd = 
      "(define load-if-present (lambda (f) (if (file-exists? f) (load f))))"
      ++ "(define (component-library dir . rst)" 
        ++ "(begin (display \"c\") (display dir) (display \"\\n\")))"
      ++ "(define (source-library dir . rst)" 
        ++ "(begin (display \"s\") (display dir) (display \"\\n\")))"
      ++ "(load (build-path geda-rc-path \"system-gafrc\"))"
      ++ "(load-if-present \"" ++ pwd ++ "/gafrc\")"

-- |Looks up a file in a list of library directories, dies if DNE
fullPathLookup :: FilePath -> [FilePath] -> IO FilePath
fullPathLookup fn [] = throwIO $ ErrorCall 
                       $ ("Could not find file " ++ fn ++ " in libraries")
fullPathLookup fn (lib:libs) = do 
  check <- fileExist (lib </> fn)
  if check then return (lib </> fn)
    else fullPathLookup fn libs

-- |Expands embedded components given a list of libraries
embedComps :: [FilePath] -> GSchem -> IO (GSchem)
embedComps libs obj@(C {..})
  | emb_comp /= [] = return obj
  | otherwise = 
    do new_emb_comp <- getGSchematic =<< fullPathLookup basename libs
       return C {emb_comp = new_emb_comp, ..}
  
embedComps _ obj = return obj

-- |Expands the sources for a component object
hCompSources :: [FilePath] -> GSchem -> IO GSchem
hCompSources libs obj@(C {..}) = do
  let source_basenames = getAllAtts obj "source"
  source_fullnames <- mapM (`fullPathLookup` libs) source_basenames
  new_sources <- mapM getGSchematic source_fullnames
  return C {sources = new_sources, ..}
    
hCompSources _ obj = return obj

-- |Recursively expands the hierarchy underneath a component.  
-- Components with a "graphical" attribute are not followed.
expandHComps :: [FilePath] -> [FilePath] -> GSchem -> IO GSchem
expandHComps clibs slibs obj@(C {..}) 
  | sources /= [] = return obj
  | getAtt obj "graphical" /= Nothing = return obj
  | otherwise = do C {..} <- hCompSources slibs =<< embedComps clibs obj
                   expanded_sources <- expandHierarchies clibs slibs sources
                   return C {sources = expanded_sources, ..}
  
expandHComps _ _ obj = return obj

-- |Expands a list of schematics into a list of hierarchical schematics
expandHierarchies :: [FilePath] -> [FilePath] -> [[GSchem]] -> IO [[GSchem]]
expandHierarchies clibs slibs gschems = 
  forM gschems (mapM $ expandHComps clibs slibs)