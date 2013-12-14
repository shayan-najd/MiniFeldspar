{-# OPTIONS_GHC -Wall #-}
module UntypedADT where
 
import ErrorMonad
import ADT (Var(..),Val(..),get,app,add)

-- ADT representation (Debruijn indices) of the lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp =
    Con Int
  | Var Var
  | Abs Exp 
  | App Exp Exp 
  | Add Exp Exp 
  deriving Eq
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    

-- Evaluation of expressions under specific environment of values 
evl :: Exp -> [Val] -> ErrM Val
evl (Con i)     _ = return (Num i)
evl (Var x)     r = get x r
evl (Abs eb)    r = return (Fun (\ va -> case evl eb (va : r) of 
                                    Rgt vb -> vb
                                    Lft s  -> error s))
evl (App ef ea) r = do vf <- evl ef r
                       va <- evl ea r
                       vf `app` va 
evl (Add el er) r = do vl <- evl el r 
                       vr <- evl er r      
                       vl `add` vr
 
-- An example expression doubling the input number
dbl :: Exp
dbl = Abs (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Exp
compose = Abs (Abs (Abs  (Var (Suc (Suc Zro)) 
                          `App` 
                          (Var (Suc Zro) 
                           `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (Num 4) -> True
          _           -> False) 

instance Show Exp where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs eb)              = "(\\ " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
