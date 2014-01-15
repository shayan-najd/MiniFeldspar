{-# OPTIONS_GHC -Wall #-}
module ADT where
 
-- ADT representation.
-- Simply-typed lambda calculus with de Bruijn indices,
-- with integer constants, and addition.
-- Philip Wadler and Shayan Najd, November 2013

import ErrorMonad

data Exp =
    Con Int
  | Var Var
  | Abs Typ Exp 
  | App Exp Exp 
  | Add Exp Exp 
  deriving Eq
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    

-- Variables are represented as natural numbers
data Var =
    Zro
  | Suc Var
  deriving Eq
             
instance Show Var where
  show = show . natToInt

natToInt :: Var -> Int
natToInt Zro     = 0
natToInt (Suc n) = (natToInt n) + 1
 

-- Types
data Typ =
    Int  
  | Arr Typ Typ
  deriving Eq
           
-- Equality between types
(===) :: Monad m => Typ -> Typ -> m ()
Int         === Int           = return ()
(Arr t1 t2) === (Arr t1' t2') = do t1 === t1'  
                                   t2 === t2'
_           === _             = fail "Type Error!" 

-- Extraction of values form environment
get :: Monad m => Var -> [a] -> m a
get Zro     (x:_)  = return x
get (Suc n) (_:xs) = get n xs
get _       []     = fail "Scope Error!"

-- Values
data Val =
    Num Int
  | Fun (Val -> Val)
    
-- Application of two values
app :: Monad m => Val -> Val -> m Val
app (Fun f) v  = return (f v)
app _       _  = fail "Type Error!"

-- Addition of two values
add :: Monad m => Val -> Val -> m Val
add (Num i) (Num j) = return (Num (i + j))
add _       (_    ) = fail "Type Error!"

-- Evaluation of expressions under specific environment of values 
run :: Exp -> [Val] -> ErrM Val
run (Con i)     _ = return (Num i)
run (Var x)     r = get x r
run (Abs _  eb) r = return (Fun (\ v -> case run eb (v : r) of 
                                    Rgt vr -> vr
                                    Lft s  -> error s))
run (App ef ea) r = do vf <- run ef r
                       va <- run ea r
                       vf `app` va 
run (Add el er) r = do vl <- run el r 
                       vr <- run er r      
                       vl `add` vr

-- Typechecking and returning the type, if successful
chk :: Monad m => Exp -> [Typ] -> m Typ 
chk (Con _)     _ = return Int
chk (Var x)     r = get x r
chk (Abs ta eb) r = do tr <- chk eb (ta : r)
                       return (ta `Arr` tr)
chk (App ef ea) r = do ta `Arr` tr <- chk ef r
                       ta'         <- chk ea r
                       ta === ta' 
                       return tr
chk (Add el er) r = do tl <- chk el r
                       tr <- chk er r
                       tl === Int
                       tr === Int
                       return Int

-- An example expression doubling the input number
dbl :: Exp
dbl = Abs Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Typ -> Typ -> Typ -> Exp
compose s t u = Abs (Arr t u) 
                (Abs (Arr s t) 
                 (Abs s
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose Int Int Int `App` dbl `App` dbl) `App` (Con 1)

-- Two test cases
test :: Bool
test =  (chk four [] == Just Int) 
        && 
        (case run four [] of 
            Rgt (Num 4) -> True
            _           -> False)
