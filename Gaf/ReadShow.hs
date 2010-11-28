module Gaf.ReadShow where
import Gaf
import Data.List (intercalate)

instance Show Att where 
  show (Att (key,value) x y c s v show_nv ang align) = 
     "T " ++  
     (intercalate " " (map show [x,y,c,s,v,show_nv,ang,align])) ++ " 1 \n" ++  
     key ++ "=" ++ value
