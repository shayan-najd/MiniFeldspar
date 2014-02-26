{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module PrettyPrinter.Type.STLC where

import Type.STLC

instance Show Typ where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 