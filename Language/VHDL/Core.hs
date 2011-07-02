{-# OPTIONS_GHC -XRecordWildCards #-}
module Language.VHDL.Core where

{- The following presents an Algebraic Data Type (ADT) for parsing a tiny
   fragment of VHDL syntax. In general, the entirety of the VHDL grammar
   is not context free, so to give it as an ADT is extremely difficult. -}

data Entity =
  Entity { identifier :: ID
         , generic :: [Generic]
         , port ::  [Port]
         , workdir :: Maybe FilePath
         , architecture :: Maybe Architecture }
  deriving (Show,Eq)

type ID = String
type Type = String
type Value = String
data DIR = IN | OUT | INOUT
         deriving (Show,Eq)
type Port = (ID, DIR, Type, Maybe Value)
type Generic = (ID,Type,Maybe Value)
type Architecture = String

emptyEntity :: Entity
emptyEntity =   Entity { identifier = ""
                       , generic = []
                       , port = []
                       , workdir = Nothing
                       , architecture = Nothing }