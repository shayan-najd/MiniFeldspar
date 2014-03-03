module Expression.STLC.ADTChurch where

import Data.Nat
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..))

data Exp t =
    Con Integer
  | Var Nat
  | Abs t       (Exp t) 
  | App (Exp t) (Exp t) 
  | Add (Exp t) (Exp t) 
  deriving (Eq , Functor, Foldable, Traversable)
 