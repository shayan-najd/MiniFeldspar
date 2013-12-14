{-# OPTIONS_GHC -Wall #-}
module ADT where
 
import ErrorMonad

-- ADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- expressions with Integer constants and a built-in addition operator
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
 
-- Types
data Typ =
    Int  
  | Arr Typ Typ
  deriving Eq
           
-- Equality between types
(===) :: Typ -> Typ -> ErrM ()
t1 === t2 
  | t1 == t2  = return ()
  | otherwise = fail "Type Error!" 

-- Extraction of values from environment
get :: Monad m => Var -> [a] -> m a
get Zro     (x : _ ) = return x
get (Suc n) (_ : xs) = get n xs
get _       []       = fail "Scope Error!"

-- Values
data Val =
    Num Int
  | Fun (Val -> Val)
    
-- Application of two values
app :: Val -> Val -> ErrM Val
app (Fun f) v  = return (f v)
app _       _  = fail "Type Error!"

-- Addition of two values
add :: Val -> Val -> ErrM Val
add (Num i) (Num j) = return (Num (i + j))
add _       _       = fail "Type Error!"

-- Evaluation of expressions under specific environment of values 
evl :: Exp -> [Val] -> ErrM Val
evl (Con i)     _ = return (Num i)
evl (Var x)     r = get x r
evl (Abs _  eb) r = return (Fun (\ va -> case evl eb (va : r) of 
                                    Rgt vb -> vb
                                    Lft s  -> error s))
evl (App ef ea) r = do vf <- evl ef r
                       va <- evl ea r
                       vf `app` va 
evl (Add el er) r = do vl <- evl el r 
                       vr <- evl er r      
                       vl `add` vr

-- Typechecking and returning the type, if successful
chk :: Exp -> [Typ] -> ErrM Typ 
chk (Con _)     _ = return Int
chk (Var x)     r = get x r
chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                       return (ta `Arr` tb)
chk (App ef ea) r = do ta `Arr` tb <- chk ef r
                       ta'         <- chk ea r
                       ta === ta' 
                       return tb
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
compose ta tb tc = Abs (Arr tb tc) 
                (Abs (Arr ta tb) 
                 (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose Int Int Int `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (Num 4) -> True
          _           -> False) 
       && (chk four [] == Rgt Int)

instance Show Exp where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs t eb)            = "(\\" ++ show t ++ " -> " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 

instance Show Typ where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 

instance Show Var where
 show = ("x"++) . show . int

int :: Var -> Int  
int Zro     =  0
int (Suc v) =  1 + int v 
