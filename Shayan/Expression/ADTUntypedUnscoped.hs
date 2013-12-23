{-# OPTIONS_GHC -Wall #-}
module Expression.ADTUntypedUnscoped where
 
-- ADT representation (Debruijn indices) of the lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp a =
    Con Integer
  | Var a
  | Abs a       (Exp a) 
  | App (Exp a) (Exp a)
  | Add (Exp a) (Exp a)
  deriving Eq
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    

instance Show a => Show (Exp a) where 
  show (Con i)               = show i 
  show (Var x)               = show x 
  show (Abs x eb)            = "(\\ " ++ show x ++ " ->" ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
