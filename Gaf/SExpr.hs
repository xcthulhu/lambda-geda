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
                   ++    [show $ key ++ "=" ++ value]
                 
instance SExpr GSchem where
  sexpr L {..} = sx $  ["L"]
                 ++ ms [x1, y1, x2, y2, color, line_width, capstyle, dashstyle, 
                        dashlength, dashspace]
                 ++ [sexpr atts]