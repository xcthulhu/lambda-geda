{-# OPTIONS_GHC -XRecordWildCards -XFlexibleInstances 
                -XTypeSynonymInstances                          #-}
module Geda.ShowGSchem (showGSchem) where

import Geda.Core
import Data.List (intercalate)
import Control.Monad (join)

{- This code is largely derivative of the S-Expression converter... 
   Perhaps they could be factored together with some kind of closure? 
   Worrying about finding common factors does not appear to be more productive
   than actually programming redundantly. 

   Arguably using typeclasses for this sort of trivial overloading is
   bad practice. -}

class GSchemShow a where
  showGSchem :: a -> String

instance GSchemShow [GSchem] where
  showGSchem gs = join $ map showGSchem gs

instance GSchemShow [Att] where
  showGSchem [] = ""
  showGSchem atts = "{\n" ++ (join $ map showGSchem atts) ++ "}\n"

{- This helper function pads with spaces and places a newline -}
sx :: [String] -> String
sx ss = intercalate " " ss ++ "\n"

{- "map (show .)": it is used often so it gets a helper function -}
ms :: (Show a) => [a] -> [String]
ms = map show

{- "perhaps i" makes a list containing a string showing an integer value,
   provided i is a valid (ie, not equal to -1) and otherwise returns the
   the empty list -}
perhaps :: Int -> [String]
perhaps i = case i of {(-1) -> []; _ -> [show i]}

instance GSchemShow Att where
  showGSchem Att {..} = 
    sx (["T"] ++ ms [x1, y1, color, size, visibility, show_name_value,
                     angle, alignment] 
              ++ perhaps num_lines)
    ++ key ++ "=" ++ value ++ "\n"

instance GSchemShow GSchem where
  showGSchem (Basename _) = ""
  showGSchem (Pathname _) = ""
  showGSchem Version {..} =
    sx (["v"] ++ ms [version] ++ perhaps fileformat_version)

  showGSchem L {..} = 
    sx (["L"] ++ ms [x1, y1, x2, y2, color, line_width, capstyle, dashstyle, 
                     dashlength, dashspace])
    ++ showGSchem atts
  
  showGSchem G {..} = 
    sx (["G"] ++ ms [x1, y1, box_width, box_height, angle, ratio, mirrored, 
                     embedded]) ++ "\n"
              ++ filename ++ "\n"
              ++ if (embedded==1) then enc_data ++ "\n.\n"
                                  else ""
    ++ showGSchem atts

  showGSchem B {..} = 
    sx (["B"] ++ ms [x1, y1, box_width, box_height, color, line_width, 
                     capstyle, dashstyle, dashlength, dashspace, filltype, 
                     fillwidth, angle1, pitch1, angle2, pitch2])
    ++ showGSchem atts
                 
  showGSchem V {..} = 
    sx (["V"]++ ms [x1, y1, radius, color, line_width, capstyle, dashstyle, 
                    dashlength, dashspace, filltype, fillwidth, angle1, pitch1, 
                    angle2, pitch2])
    ++ showGSchem atts

  showGSchem A {..} = 
    sx (["A"] ++ ms [x1, y1, radius, startangle, sweepangle, color, line_width, 
                     capstyle, dashstyle, dashlength, dashspace])
    ++ showGSchem atts
  
  showGSchem T {..} = 
    sx (["T"] ++ ms [x1, y1, color, size, visibility, show_name_value, angle, 
                     alignment]
              ++ perhaps num_lines)
    ++ join [ ln ++ "\n" | ln <- text]
    ++ showGSchem atts
    
  showGSchem N {..} = 
    sx (["N"] ++ ms [x1, y1, x2, y2, color]) ++ showGSchem atts

  showGSchem U {..} = 
    sx (["U"] ++ ms [x1, y1, x2, y2, color, ripperdir]) ++ showGSchem atts
  
  showGSchem P {..} = 
    sx (["P"] ++ ms [x1, y1, x2, y2, color]
              ++ perhaps pintype 
              ++ perhaps whichend)
    ++ showGSchem atts

  showGSchem C {..} = 
    sx (["C"] ++ ms [x1, y1, selectable, angle, mirror] ++ [basename])
    ++ case emb_comp of { [] -> ""
                        ; _  -> "[\n" ++ showGSchem emb_comp ++ "]\n" }
    ++ showGSchem atts

  showGSchem H {..} = 
    sx (["H"] ++ ms [color, line_width, capstyle, dashstyle, dashlength, 
                     dashspace, filltype, fillwidth, angle1, pitch1, angle2,
                     pitch2, num_lines])
    ++ showGSchem path
    ++ showGSchem atts

  showGSchem (F att) = showGSchem att

instance GSchemShow Int where
  showGSchem = show

instance GSchemShow a => GSchemShow (a, a) where
  showGSchem (x,y) = showGSchem x ++ "," ++ showGSchem y

instance GSchemShow a => GSchemShow (a, a, a) where
  showGSchem (x,y,z) = intercalate "," $ map showGSchem [x,y,z]

instance GSchemShow Path where
  showGSchem (MM args) = sx (["M"] ++ map showGSchem args) ++ "\n"
  showGSchem (Mm args) = sx (["m"] ++ map showGSchem args) ++ "\n"
  showGSchem (LL args) = sx (["L"] ++ map showGSchem args) ++ "\n"
  showGSchem (Ll args) = sx (["l"] ++ map showGSchem args) ++ "\n"
  showGSchem (CC args) = sx (["C"] ++ map showGSchem args) ++ "\n"
  showGSchem (Cc args) = sx (["c"] ++ map showGSchem args) ++ "\n"
  showGSchem Z = "Z\n"

instance GSchemShow [Path] where
  showGSchem path = intercalate "\n" (map showGSchem path) ++ "\n"
