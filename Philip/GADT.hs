{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module GADT where

-- GADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- with integer constants and addition.
-- Philip Wadler and Shayan Nadj, November 2013

data Exp e a where
  Con :: Int -> Exp e Int
  Add :: Exp e Int -> Exp e Int -> Exp e Int
  Var :: Var e a -> Exp e a
  Abs :: Typ a -> Exp (e,a) b -> Exp e (a -> b)
  App :: Exp e (a -> b) -> Exp e a -> Exp e b

-- Variables
data Var e a where
  Zro :: Var (e,a) a
  Suc :: Var e a -> Var (e,b) a

-- Types (Singleton)
data Typ a where
  Int :: Typ Int
  Arr :: Typ a -> Typ b -> Typ (a -> b)

-- Environment (Singleton)
data Env e where
  Emp :: Env ()
  Ext :: Env e -> Typ a -> Env (e,a)

-- Extraction of values form environment
get :: Var e a -> e -> a
get Zro     (_ ,x)      = x
get (Suc n) (xs,_)      = get n xs

-- Extraction of values form environment with singletons
gets :: Var e a -> Env e -> Typ a
gets Zro     (Ext _  x) = x
gets (Suc n) (Ext xs _) = gets n xs
gets _       Emp        = error "Impossible!" 
 
-- Evaluation of expressions under specific environment of values 
run :: Exp e a -> e -> a
run (Con i)     _ = i
run (Var x)     r = get x r
run (Abs _  eb) r = \v -> run eb (r,v)
run (App ef ea) r = run ef r $ run ea r
run (Add el er) r = run el r + run er r

-- Typechecking and returning the type, if successful
chk :: Exp e a -> Env e -> Typ a
chk (Con _)     _ = Int
chk (Var x)     r = gets x r
chk (Abs ta eb) r = ta `Arr` chk eb (r `Ext` ta)
chk (App ef _ ) r = case chk ef r of 
  Arr _ tr -> tr
chk (Add _  _ ) _ = Int 
 
-- An example expression doubling the input number                    
dbl :: Exp () (Int -> Int)
dbl = Abs Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Typ a -> Typ b -> Typ c -> Exp () ((b -> c) -> (a -> b) -> (a -> c))
compose s t u = Abs (t `Arr` u) (Abs (s `Arr` t) (Abs s
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp () Int
four = (compose Int Int Int `App` dbl `App` dbl) `App` (Con 1)
 
-- Two test cases
test :: Bool
test = (case chk four Emp of 
          Int -> True)
       &&
       (run four () == 4)
