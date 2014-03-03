module PrettyPrinter.Type.Feldspar where

import Type.Feldspar

instance Show Typ where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (ta@(_  `Arr` _) `Arr` tb) = "(" ++ show ta ++ ") -> " ++ show tb 
  show (ta  `Arr` tb)             = show ta ++ " -> " ++ show tb 
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t