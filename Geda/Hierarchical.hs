{-# OPTIONS_GHC -XRecordWildCards -XTypeSynonymInstances #-}
module Geda.Hierarchical where

import Geda.Core
import Geda.Parser (readGSchem)
import Geda.ShowGSchem (showGSchem)
import System (exitFailure)
import System.FilePath (splitFileName, (</>), equalFilePath)
import System.Directory (canonicalizePath)
import System.Posix.Files
import System.IO
import System.Process (readProcess)
import Data.List (lines, nubBy, splitAt, find)
import Control.Monad (liftM2, mapM, forM, join)

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
      Left err -> hPutStrLn stderr (fn ++ " FAILED!\nReason: " ++ show err)
                  >> exitFailure
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

-- |Get the basename of a schematic
baseName :: [GSchem] -> String
baseName gschem = bname
  where
    Just (Basename bname) = find bases gschem
    bases (Basename _) = True
    bases _ = False

-- |Returns the component and source libraries loaded by calling gnetlist
getLibraries :: IO ([FilePath],[FilePath]) 
getLibraries = do
  raw_list <- readProcess 
              "gnetlist" ["-q"             -- Be quiet
                         ,"-l","/dev/stdin" -- Run a command we feed
                         ,"-g","geda"     -- Pretend to netlist geda
                         ,"-o","/dev/null" -- But throw away output
                         ,"/dev/null"     -- Don't use any input files
                         ] schemeProg
  let entries = map (splitAt 1) $ reverse . lines $ raw_list
  components <- mapM (canonicalizePath . snd) 
                $ filter (\ (x,_) -> (x == "c")) entries
  sources <- mapM (canonicalizePath . snd) 
             $ filter (\ (x,_) -> (x == "s")) entries
  -- Return the lists with duplicates thrown away
  return (nubBy equalFilePath components,
          nubBy equalFilePath sources)
  where
    {- This is the scheme program we are handing gnetlist to run (using 
       stdin). It loads all of the libraries, and prints them to 
       "/dev/stdout", one library per line.  The sort of library is
       identified by the first letter. -}
    schemeProg = 
      "(define load-if-present (lambda (f) (if (file-exists? f) (load f))))"
      ++ "(define (component-library dir . rst)" 
        ++ "(begin (display \"c\") (display dir) (display \"\\n\")))"
      ++ "(define (source-library dir . rst)" 
        ++ "(begin (display \"s\") (display dir) (display \"\\n\")))"
      ++ "(load (build-path geda-rc-path \"system-gafrc\"))"
      ++ "(load-if-present (build-path (getenv \"HOME\") \".gEDA/gafrc\"))"
      ++ "(load-if-present \"gafrc\")"

-- |Looks up a file in a list of library directories, dies if DNE
fullPathLookup :: FilePath -> [FilePath] -> IO FilePath
fullPathLookup fn [] = hPutStrLn stderr ("Could not find file " ++ fn 
                                         ++ " in libraries") >> exitFailure
fullPathLookup fn (lib:libs) = do 
  check <- fileExist (lib </> fn)
  if check then return (lib </> fn)
    else fullPathLookup fn libs

-- |Expands embedded components given a list of libraries
expandEmbeddedComponent :: [FilePath] -> GSchem -> IO (GSchem)
expandEmbeddedComponent libs (C {..}) = do
  new_emb_comp <- getGSchematic =<< fullPathLookup basename libs
  return C {emb_comp = new_emb_comp, ..}
  
expandEmbeddedComponent _ obj = return obj

-- | Gets the values for a key from a list of attributes
getAllAtts :: GSchem -> String -> [String]
getAllAtts obj mykey =
    map (\ Att {..} -> value) 
    $ filter (\ Att {..} -> (key == mykey)) 
    $ attributes obj

-- | Gets the first value for a key from a list of attributes
getAtt :: GSchem -> String -> Maybe String
getAtt obj mykey =
  let vals = getAllAtts obj mykey in
  if (vals == []) then Nothing else Just (head vals)

-- |Expands the sources for a component object
expandSources :: [FilePath] -> GSchem -> IO GSchem
expandSources libs obj@(C {..}) =
  if (getAtt obj "graphical" == Just "1") then return obj
  else do
    let source_basenames = getAllAtts obj "source"
    source_fullnames <- mapM (`fullPathLookup` libs) source_basenames
    new_sources <- mapM getGSchematic source_fullnames
    return C {sources = new_sources, ..}
    
expandSources _ obj = return obj

-- |Recursively expands the hierarchy underneath a component
-- graphical components are omitted
expandHComps :: [FilePath] -> [FilePath] -> GSchem -> IO GSchem
expandHComps clibs slibs comp@(C {..}) = do
  C {..} <- expandSources slibs =<< 
            expandEmbeddedComponent clibs comp
  expanded_sources <- expandHierarchies clibs slibs sources
  return C {sources = expanded_sources, ..}
  
expandHComp _ _ obj = return obj

-- |Expands a list of schematics into a list of hierarchical schematics
expandHierarchies :: [FilePath] -> [FilePath] -> [[GSchem]] -> IO [[GSchem]]
expandHierarchies clibs slibs gschems = 
  forM gschems (mapM $ expandHComps clibs slibs)

-- |Updates a refdes attribute
updRefdesAtt :: String -> Att -> Att
updRefdesAtt rd att@(Att {..}) 
  | key == "refdes" = Att {value = rd </> value, ..}
  | otherwise = att

-- |Recursively changes refdes and basenames to reflect their refdes parents
refdesRename :: String -> GSchem -> GSchem
refdesRename rd (Basename bn) = Basename $ rd ++ "-" ++ bn
refdesRename rd obj@(C {..}) = 
  let (Just rd') = getAtt obj "refdes" 
      new_sources = (map.map) (refdesRename rd') sources in
  fmap (updRefdesAtt rd) $ C {sources = new_sources, ..}
refdesRename rd obj = fmap (updRefdesAtt rd) obj

-- |Map a transformation over a hierarchical GSchem object
mapGSchem :: (GSchem -> GSchem) -> GSchem -> GSchem
mapGSchem f C {..} = f C {sources = (map.map) f sources, 
                          emb_comp = map f emb_comp, ..}
mapGSchem f obj = f obj

-- |Make a hierarchical component graphical
makeHCompGraphical :: GSchem -> GSchem
makeHCompGraphical obj@(C {..})
  | sources == [] = obj
  | otherwise = let 
    grphatt = (Att {color=0, size=12, visibility=0, show_name_value=0, 
                   angle=0, alignment=0, num_lines=1, key="graphical", 
                   value="1", ..})
    in C {atts = grphatt:atts, ..}
makeHCompGraphical obj = obj

-- |Recursively extracts all subschematics
subGSchematics :: GSchem -> [[GSchem]]
subGSchematics C {..} = 
  sources ++ (concatMap subGSchematics $ join sources)
subGSchematics _ = []

-- |Flattens a list of hierarchical schematics, rendering all
-- child hierarchical components graphical
flattenHierarchies :: [[GSchem]] -> [[GSchem]]
flattenHierarchies gschems = do
  g <- gschems
  let g' = do { obj <- g 
              ; let obj' = mapGSchem makeHCompGraphical 
                           $ refdesRename "" obj
              ; return obj' }
  new_schem <- g':(concatMap subGSchematics g')
  return new_schem