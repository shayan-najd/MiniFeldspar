module Expression.STLC.ADTUntypedDebruijn where

import Data.Nat
 
data Exp =
    Con Integer
  | Var Nat
  | Abs Exp 
  | App Exp Exp 
  | Add Exp Exp 
  deriving Eq
 