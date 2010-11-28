module Gaf where

{- The data structures below characterize the common specificiation of 
   .sch and .sym files which can be found here:
   http://www.geda.seul.org/wiki/geda:file_format_spec

   We have a few points to add:

   (1) Note that while the official specification does not say this
       explicitly, gschem data structures form trees.  This is because
       components may have embedded schematics associated with them.

   (2) Also note that unofficially, there can be "floating attributes"
       in a schematic; these have a constructor "F" associated with them
-}

data GSchem = 
   Version {version, fileformat_version :: Int}
 | L {x1, y1, x2, y2, color, line_width, capstyle, dashstyle, dashlength, 
      dashspace :: Int, atts :: [Att]}
 | G {x1, y1, box_width, box_height, angle, ratio, mirrored, embedded :: Int,
      filename, enc_data :: String, atts :: [Att]}
 | B {x1, y1, box_width, box_height, color, line_width, capstyle, dashstyle, 
      dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, 
      pitch2 :: Int, atts :: [Att]}
 | V {x1, y1, radius, color, line_width, capstyle, dashstyle, dashlength, 
      dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2 :: Int, 
      atts :: [Att]}
 | A {x1, y1, radius, startangle, sweepangle, color, line_width, capstyle, 
      dashstyle, dashlength, dashspace :: Int, atts :: [Att]}
 | T {x1, y1, color, size, visibility, show_name_value, angle, alignment, 
      num_lines :: Int, text :: [String], atts :: [Att]}  
 | N {x1, y1, x2, y2, color :: Int, atts :: [Att]}
 | U {x1, y1, x2, y2, color, ripperdir :: Int, atts :: [Att]}
 | P {x1, y1, x2, y2, color, pintype, whichend :: Int, atts :: [Att]}
 | C {x1, y1, selectable, angle, mirror :: Int, basename :: String, 
      subcomp :: [GSchem], atts :: [Att]}
 | H {color, line_width, capstyle, dashstyle, dashlength, dashspace, filltype, 
      fillwidth, angle1, pitch1, angle2, pitch2, num_lines :: Int, 
      path :: [Path], atts :: [Att]}
 | F Att
  deriving (Show, Eq, Ord)

{- Attributes are just like text, only they have a key-value pair at the end
   Note: we have eliminated the "num_lines" field because it is meaningless
         in the context of attributes.                                      -}

data Att = Att {x1_, y1_, color_, size_, visibility_, show_name_value_, angle_, 
                alignment_, num_lines_ :: Int, key, value :: String} 
  deriving (Show, Eq, Ord)

{- Paths are a subset of the SVG standard.  See the following for details:
   http://www.geda.seul.org/wiki/geda:file_format_spec#path_data           -}

data Path = MM [(Int, Int)]
          | Mm [(Int, Int)]
          | LL [(Int, Int)]
          | Ll [(Int, Int)]
          | CC [((Int, Int),(Int,Int),(Int,Int))]
          | Cc [((Int, Int),(Int,Int),(Int,Int))]
          | Z
  deriving (Show, Eq, Ord)
