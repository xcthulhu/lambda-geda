{-# OPTIONS_GHC -XRecordWildCards -XFlexibleInstances #-}
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

{- We use record wildcards extensively.  You may read about them here:
   http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#record-wildcards -}

type SExpression = String

class SExpr a where
  sexpr :: a -> SExpression

instance (SExpr a) => SExpr [a] where
  sexpr xs = "'(" ++ (intercalate " " $ map sexpr xs) ++ ")"

{- This helper function pads a list of string spaces and returns an 
   SExpression -}
sx :: [String] -> SExpression
sx ss = "(" ++ intercalate " " ss ++ ")"

{- This is the quoted version of sx; used for lists without identifiers -}
ql :: [String] -> SExpression
ql ss = "'" ++ sx ss

{- "map (show .)": it is used often so it gets a helper function -}
ms :: (Show a) => [a] -> [String]
ms = map show

{- "perhaps i" makes a list containing a string showing an integer value,
   provided i is a valid (ie, not equal to -1) and otherwise returns the
   the empty list -}
perhaps :: Int -> [String]
perhaps i = case i of {(-1) -> []; _ -> [show i]}

{- FIXME!  All of this line manipulation isn't very fast.  Maybe migrate 
   everything to the WriterMonad?  But then again, more crazy Haskell 
   constructions = harder to understand, and it will only shave off a few tenths 
   of a second in practice. -}

instance SExpr Att where
  sexpr Att {..} = sx $  ["T"]
                   ++ ms [x1_, y1_, color_, size_, visibility_, show_name_value_, 
                          angle_, alignment_] 
                   ++ perhaps num_lines_
                   ++ [ql $ ms [key ++ "=" ++ value]]
      where
        num_lines_perhaps = case num_lines_ of {(-1) -> [] ; _ -> [num_lines_]}

instance SExpr GSchem where
  sexpr Version {..} = sx $  ["v"]
                       ++ ms [version]
                       ++ perhaps fileformat_version

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
                        alignment]
                 ++ perhaps num_lines
                 ++ [ql $ ms text]
                 ++ [sexpr atts]

  sexpr N {..} = sx $  ["N"]
                 ++ ms [x1, y1, x2, y2, color]
                 ++ [sexpr atts]

  sexpr U {..} = sx $  ["U"]
                 ++ ms [x1, y1, x2, y2, color, ripperdir]
                 ++ [sexpr atts]
  
  sexpr P {..} = sx $  ["P"]
                 ++ ms [x1, y1, x2, y2, color]
                 ++ perhaps pintype
                 ++ perhaps whichend
                 ++ [sexpr atts]

  sexpr C {..} = sx $  ["C"]
                 ++ ms [x1, y1, selectable, angle, mirror]
                 ++ ms [basename]
                 ++ [sexpr subcomp]
                 ++ [sexpr atts]

  sexpr H {..} = sx $  ["H"]
                 ++ ms [color, line_width, capstyle, dashstyle, dashlength, 
                        dashspace, filltype, fillwidth, angle1, pitch1, angle2, 
                        pitch2, num_lines]
                 ++ [sexpr path]
                 ++ [sexpr atts]

  sexpr (F att) = sexpr att

instance SExpr Int where
  sexpr = show

instance SExpr a => SExpr (a, a) where
  sexpr (x,y) = sexpr [x,y]

instance SExpr a => SExpr (a, a, a) where
  sexpr (x,y,z) = sexpr [x,y,z]

instance SExpr Path where
  sexpr (MM args) = sx $ ["M"] ++ [sexpr args]
  sexpr (Mm args) = sx $ ["m"] ++ [sexpr args]
  sexpr (LL args) = sx $ ["L"] ++ [sexpr args]
  sexpr (Ll args) = sx $ ["l"] ++ [sexpr args]
  sexpr (CC args) = sx $ ["C"] ++ [sexpr args]
  sexpr (Cc args) = sx $ ["c"] ++ [sexpr args]
  sexpr Z = sx $ ["Z"]