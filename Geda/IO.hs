{-# OPTIONS_GHC -XRecordWildCards #-}
module Geda.IO where

import Control.Exception
import Control.Monad (liftM2, mapM, forM)
import Data.List (lines, nubBy)
import List (find)
import Geda.Core
import Geda.Parser (readGSchem)
import Geda.ShowGSchem (showGSchem)
import Geda.Hierarchical
import System.Directory (canonicalizePath,getCurrentDirectory)
import System.FilePath (splitFileName, (</>), equalFilePath)
import System.IO
import System.Posix.Files (fileExist)
import System.Process (readProcess)

-- |Open and parse a schematic from a FilePath. 
-- |Redirects a FilePath of "-" to stdin
fnGetGSchematic :: FilePath -> IO [GSchem]
fnGetGSchematic fn = do
  fh <- if (fn == "-") then return stdin
                       else openFile fn ReadMode
  fh <- openFile fn ReadMode
  contents <- hGetContents fh
  sch <- tryParse fn contents
  if (fn == "-") then return ()
                 else hClose fh
  return sch
  where
    tryParse fn x = case readGSchem x of
      Left err -> throwIO $ ErrorCall 
                  $ "Parse of " ++ fn ++ " FAILED!\nReason: " ++ show err
      -- Prepend the Filename for reference purposes, provided input isn't specified as stdin
      Right sch -> do
        if (fn == "-") 
          then return sch
          else do {
              let (path,base) = splitFileName fn
            ; return $ (Basename base):(Dirname path):sch }

-- |Get the basename metadata of a schematic
baseName :: [GSchem] -> String
baseName gschem = bname
  where
    bname = maybe "" (\(Basename s) -> s) $ find base gschem
    base (Basename _) = True
    base _ = False
    
-- |Get the dirname metadata of a schematic
dirName :: [GSchem] -> String
dirName gschem = dname
  where
    dname = maybe "" (\(Dirname s) -> s) $ find dir gschem
    dir (Basename _) = True
    dir _ = False

-- |Get encoded FilePath metadata from a schematic
fullPath :: [GSchem] -> FilePath
fullPath gschem = (dirName gschem) ++ (baseName gschem)

-- |Print a schematic to a FilePath.
-- |Redirects a FilePath of "-" to stdout
-- |Redirects a FilePath of "_" to use Dirname/Basename metadata to reconstruct the FilePath 
fnPutGSchematic :: FilePath -> [GSchem] -> IO ()
fnPutGSchematic fn gschem = do
  fh <- case fn of { "-" -> return stdout 
                   ; "_" -> openFile (fullPath gschem) WriteMode
                   ; _ -> openFile fn WriteMode }
  hPutStr fh $ showGSchem gschem
  case fn of { "-" -> return () 
             ; _ -> hClose fh }
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
    do new_emb_comp <- fnGetGSchematic =<< fullPathLookup basename libs
       return C {emb_comp = new_emb_comp, ..}
  
embedComps _ obj = return obj

-- |Expands the sources for a component object
hCompSources :: [FilePath] -> GSchem -> IO GSchem
hCompSources libs obj@(C {..}) = do
  let source_basenames = getAllAtts obj "source"
  source_fullnames <- mapM (`fullPathLookup` libs) source_basenames
  new_sources <- mapM fnGetGSchematic source_fullnames
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
