{-# OPTIONS_GHC -XRecordWildCards #-}
module Geda.Hierarchical where

import Control.Monad (join)
import Data.List (find, isPrefixOf)
import Geda.Core
import System.FilePath ((</>))

{- This module contains pure functions for manipulating Hierarchical 
   schematics -}

-- |Get the basename of a schematic
baseName :: [GSchem] -> String
baseName gschem = bname
  where
    Just (Basename bname) = find bases gschem
    bases (Basename _) = True
    bases _ = False

-- |Gets the all values for a given GSchem object indexed by an attribute 
-- key
getAllAtts :: GSchem -> String -> [String]
getAllAtts obj mykey =
    map (\ Att {..} -> value) 
    $ filter (\ Att {..} -> (key == mykey)) 
    $ attributes obj

-- |Gets the first value for a given GSchem object indexed by an attribute 
-- key
getAtt :: GSchem -> String -> Maybe String
getAtt obj mykey =
  let vals = getAllAtts obj mykey in
  if (vals == []) then Nothing else Just (head vals)

-- |Map a transformation over a hierarchical GSchem object
mapGSchem :: (GSchem -> GSchem) -> GSchem -> GSchem
mapGSchem f C {..} = f C {sources = (map.map) f sources, 
                          emb_comp = map f emb_comp, ..}
mapGSchem f obj = f obj

-- |Updates a refdes attribute
updRefdesAtt :: String -> Att -> Att
updRefdesAtt rd att@(Att {..}) 
  | rd == "" = att
  | key == "refdes" = Att {value = rd </> value, ..}
  | otherwise = att

-- |Recursively changes refdes and basenames to reflect their refdes parents
refdesRename :: String -> GSchem -> GSchem
refdesRename rd obj@(Basename bn) | rd == "" = obj
                                  | otherwise = Basename $ rd ++ "-" ++ bn
refdesRename rd obj@(C {..}) = 
  let (Just rd') = getAtt obj "refdes" 
      new_sources = (map.map) (refdesRename rd') sources in
  fmap (updRefdesAtt rd) $ C {sources = new_sources, ..}
refdesRename rd obj = fmap (updRefdesAtt rd) obj

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

-- |Unembeds components, provided that the symbol name does
-- begin with "EMBEDDED"
unembedComp :: GSchem -> GSchem
unembedComp obj@(C {..})
 | "EMBEDDED" `isPrefixOf` basename = obj
 | otherwise = C {emb_comp = [], ..}

unembedComp obj = obj

-- |Removes all embedded components from a list of hierarchies
unembedHierarchies :: [[GSchem]] -> [[GSchem]]
unembedHierarchies gschems = do
  g <- gschems
  let g' = do { obj <- g
              ; let obj' = mapGSchem unembedComp obj
              ; return obj' }
  return g'