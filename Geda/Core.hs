{-# OPTIONS_GHC -XRecordWildCards -XDeriveFunctor #-}
module Geda.Core where
import Data.Functor

{- The data structures below characterize the common specificiation of 
   .sch and .sym files which can be found here:
   http://www.geda.seul.org/wiki/geda:file_format_spec

   We have a few points to add:

   (1) Note that while the official specification does not say this
       explicitly, gschem data structures form trees.  Specifically,
       we may think of attributes as "leaves" and hierarchical components
       as "branches".  To this end, GSchemO is an instance of "Functor"

   (2) We also represent the basename/pathname metadata of schematics,
       which is not in the spec.                                         -}

data GSchemO a = 
   Basename String
 | Pathname String
 | Version {version, fileformat_version :: Int}
 | L {x1, y1, x2, y2, color, line_width, capstyle, dashstyle, dashlength, 
      dashspace :: Int, atts :: [a]}
 | G {x1, y1, box_width, box_height, angle, ratio, mirrored, 
      embedded :: Int, filename, enc_data :: String, atts :: [a]}
 | B {x1, y1, box_width, box_height, color, line_width, capstyle, dashstyle,
      dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, 
      pitch2 :: Int, atts :: [a]}
 | V {x1, y1, radius, color, line_width, capstyle, dashstyle, dashlength, 
      dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2 :: Int,
      atts :: [a]}
 | A {x1, y1, radius, startangle, sweepangle, color, line_width, capstyle, 
      dashstyle, dashlength, dashspace :: Int, atts :: [a]}
 | T {x1, y1, color, size, visibility, show_name_value, angle, alignment, 
      num_lines :: Int, text :: [String], atts :: [a]}  
 | N {x1, y1, x2, y2, color :: Int, atts :: [a]}
 | U {x1, y1, x2, y2, color, ripperdir :: Int, atts :: [a]}
 | P {x1, y1, x2, y2, color, pintype, whichend :: Int, atts :: [a]}
 | C {x1, y1, selectable, angle, mirror :: Int, basename :: String, 
      emb_comp :: [GSchemO a], sources :: [[GSchemO a]], atts :: [a]}
 | H {color, line_width, capstyle, dashstyle, dashlength, dashspace, 
      filltype, fillwidth, angle1, pitch1, angle2, pitch2, 
      num_lines :: Int, path :: [Path], atts :: [a]}
 | F a
 | Att {x1, y1, color, size, visibility, show_name_value, angle, alignment, 
        num_lines :: Int, key, value :: String}
  deriving (Functor, Show, Eq, Ord)

{- Components have two kinds of attributes: attached and inherited. 
   Inherited attributes come from the embedded symbol schematic -}
in_atts :: GSchemO a -> [a]
in_atts C {..} = map (\(F a) -> a) $ filter floating emb_comp
  where 
    floating (F _) = True
    floating _ = False

{- Some objects do not naturally have attributes, but it's convenient 
   to have an attribute function that works uniformly.  The following
   function does this. -}
attributes :: GSchemO a -> [a]
attributes (Basename _) = []
attributes (Pathname _) = []
attributes (Version {..}) = []
attributes (F att) = [att]
attributes obj@(C {..}) = atts ++ in_atts obj
attributes obj = atts obj

{- It's convenient to think of "Attribute" as a subtype of GschemO.
   However, it is not possible to express this in Haskell; so we use 
   a type synonym as a compromise. -}
type Att = GSchemO ()
type GSchem = GSchemO Att

{- Paths are a subset of the SVG standard.  See the following for details:
   http://www.geda.seul.org/wiki/geda:file_format_spec#path_data       -}

data Path = MM [(Int, Int)]
          | Mm [(Int, Int)]
          | LL [(Int, Int)]
          | Ll [(Int, Int)]
          | CC [((Int, Int),(Int,Int),(Int,Int))]
          | Cc [((Int, Int),(Int,Int),(Int,Int))]
          | Z
  deriving (Show, Eq, Ord)
