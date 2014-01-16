{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expression.STLC.ADTUntypedPolymorphic where
 
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..))

-- ADT representation (Debruijn indices) of the lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp x =
    Con Integer
  | Var x
  | Abs x       (Exp x) 
  | App (Exp x) (Exp x)
  | Add (Exp x) (Exp x)
  deriving (Eq, Functor, Foldable, Traversable)
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    

instance Show x => Show (Exp x) where 
  show (Con i)               = show i 
  show (Var x)               = show x 
  show (Abs x eb)            = "(\\ " ++ show x ++ " ->" ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
