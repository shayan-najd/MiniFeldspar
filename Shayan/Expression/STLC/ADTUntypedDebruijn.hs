{-# OPTIONS_GHC -Wall #-}
module Expression.STLC.ADTUntypedDebruijn where

import Variable.ADT
 
data Exp =
    Con Integer
  | Var Var
  | Abs Exp 
  | App Exp Exp 
  | Add Exp Exp 
  deriving Eq
 