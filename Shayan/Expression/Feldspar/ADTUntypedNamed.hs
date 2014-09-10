module Expression.Feldspar.ADTUntypedNamed
    (Exp(..)) where

import MyPrelude

data Exp x = ConI Integer
           | ConB Bool
           | ConF Float
           | Var x
           | Abs x (Exp x)
           | App (Exp x) (Exp x)
           | Cnd (Exp x) (Exp x) (Exp x)
           | Whl x (Exp x) x (Exp x) (Exp x)
           | Tpl (Exp x) (Exp x)
           | Fst (Exp x)
           | Snd (Exp x)
           | Ary (Exp x) x (Exp x)
           | Len (Exp x)
           | Ind (Exp x) (Exp x)
           | Let x (Exp x) (Exp x)
           | Cmx (Exp x) (Exp x)

deriving instance Eq x   => Eq   (Exp x)
deriving instance Show x => Show (Exp x)
deriving instance Functor     Exp
deriving instance Foldable    Exp
deriving instance Traversable Exp
