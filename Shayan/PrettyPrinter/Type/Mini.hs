{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module PrettyPrinter.Type.Mini where

import Type.Mini

instance Show Typ where                  
  show Int                        = "Int"
  show Bol                        = "Bool"
  show (Tpl tf ts)                = "(" ++ show tf ++ " , " ++ show ts ++ ")" 
  show (Ary t)                    = "Array " ++ show t