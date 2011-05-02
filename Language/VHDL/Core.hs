{-# OPTIONS_GHC -XRecordWildCards #-}
module Core where

{- The following presents an Algebraic Data Type (ADT) for parsing a tiny
   fragment of VHDL syntax. In general, the entirety of the VHDL grammar
   is not context free, so to give it as an ADT is extremely difficult. -}

data Entity =
  Entity { identifier :: String
         , generic :: [(ID,Type,Maybe Value)]
         , port ::  [(ID, Maybe DIR, Type, Maybe Value)]}
  deriving (Show)

{- All of the type synonyms are just strings. -}
type ID = String
type Type = String
type Value = String
data DIR = IN | OUT | INOUT
         deriving (Show)
