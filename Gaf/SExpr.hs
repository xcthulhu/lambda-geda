{-# OPTIONS_GHC -XRecordWildCards #-}

module Gaf.SExpr (sexpr) where

import Gaf
import Data.List (intercalate)

{- In this module we write a printer which turns GSchem structures into
   S-Expressions suitable for processing with Guile/Scheme/Lisp. 

   Note: apparently there is an existing Haskell module "Codec.Sexpr",
   which provides a uniform approach to encoding S-expressions.
   
   However, for our purposes we are catering to Scheme syntax, so for
   the purposes of simplicity we will create our own module from scratch. -}

{- The idea behind this module is that we declare a class "sexp" for
   coercing things into SExpressions.  SExpressions are themselves just
   strings. -}

type SExpression = String

class SExpr a where
  sexpr :: a -> SExpression

instance (SExpr a) => SExpr [a] where
  sexpr xs = let y = intercalate " " $ map sexpr xs in
             "(" ++ y ++ ")"

{- This helper function applies a list of functions returning strings 
   to its argument, pads with spaces and returns an SExpression -}
sx :: [String] -> SExpression
sx ss = "(" ++ intercalate " " ss ++ ")"
                  
{- "map (show .)": it is used often so it gets a helper function -}
ms :: (Show a) => [a] -> [String]
ms = map show

instance SExpr Att where
  sexpr Att {..} = sx $  ["T"]
                   ++ ms [x1_, y1_, color_, size_, visibility_, show_name_value_, 
                          angle_, alignment_, num_lines_] 
                   ++ [sx $ ms [key ++ "=" ++ value]]
                 
instance SExpr GSchem where
  sexpr Version {..} = sx $  ["v"]
                       ++ ms [version, fileformat_version]

  sexpr L {..} = sx $  ["L"]
                 ++ ms [x1, y1, x2, y2, color, line_width, capstyle, dashstyle, 
                        dashlength, dashspace]
                 ++ [sexpr atts]
  
  sexpr G {..} = sx $  ["G"]
                 ++ ms [x1, y1, box_width, box_height, angle, ratio, mirrored, 
                        embedded]
                 ++ ms [filename, enc_data]
                 ++ [sexpr atts]

  sexpr B {..} = sx $  ["B"]
                 ++ ms [x1, y1, box_width, box_height, color, line_width, 
                        capstyle, dashstyle, dashlength, dashspace, filltype, 
                        fillwidth, angle1, pitch1, angle2, pitch2]
                 ++ [sexpr atts]
                 
  sexpr V {..} = sx $  ["V"]
                 ++ ms [x1, y1, radius, color, line_width, capstyle, dashstyle, 
                        dashlength, dashspace, filltype, fillwidth, angle1, 
                        pitch1, angle2, pitch2]
                 ++ [sexpr atts]

  sexpr A {..} = sx $  ["A"]
                 ++ ms [x1, y1, radius, startangle, sweepangle, color, 
                        line_width, capstyle, dashstyle, dashlength, dashspace]
                 ++ [sexpr atts]
  
  sexpr T {..} = sx $  ["T"]
                 ++ ms [x1, y1, color, size, visibility, show_name_value, angle, 
                        alignment, num_lines]
                 ++ [sx (ms text)]
                 ++ [sexpr atts]
