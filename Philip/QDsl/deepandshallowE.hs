{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, Rank2Types #-}

module DeepAndShallow where
import Control.Applicative hiding (some)
import Data.Array

-- Combining Deep and Shallow Embedding
-- Josef Svenningsson and Philip Wadler, 12 Oct 2014
-- this is deepandshallowcpsdefault5, with FunC replaced by E

data E a where
  LitI      :: Int -> E Int
  LitF      :: Float -> E Float
  LitB      :: Bool -> E Bool
  If        :: E Bool -> E a -> E a -> E a
  While     :: (E s -> E Bool) -> (E s -> E s) -> E s -> E s
  Pair      :: E a -> E b -> E (a , b)
  Fst       :: E (a , b) -> E a
  Snd       :: E (a , b) -> E b
  Prim1     :: String -> (a -> b) -> E a -> E b
  Prim2     :: String -> (a -> b -> c) -> E a -> E b -> E c
  Value     :: a -> E a
  Variable  :: String -> E a
  Arr       :: E Int -> (E Int -> E a) -> E (Array Int a)
  ArrLen    :: E (Array Int a) -> E Int
  ArrIx     :: E (Array Int a) -> E Int -> E a

par        :: String -> String
par s      =  "(" ++ s ++ ")"

(<+>)      :: String -> String -> String
s <+> t    =  s ++ " " ++ t

ispar      :: String -> Bool
ispar s    =  head s == '(' && last s == ')'

droppar    :: String -> String
droppar s  =  init (tail s)

instance Show (E a) where
  show (LitI i)	        =  par (show i)
  show (LitF x)         =  par (show x)
  show (LitB b)	        =  par (show b)
  show (While c b i)    =  par ("while" <+> showf c <+> showf b <+> show i)
  show (If c t e)       =  par (show c <+> "?" <+> par (show t ++ ", " ++ show e))
  show (Pair a b)       =  par (show a ++ ", " ++ show b)
  show (Fst p)          =  par ("fst" <+> show p)
  show (Snd p)          =  par ("snd" <+> show p)
  show (Prim1 s f a)    =  par (s <+> show a)
  show (Prim2 s f a b)  | ispar s     =  par (show a <+> droppar s <+> show b)
                        | otherwise   =  par (s <+> show a <+> show b)
  show (Variable x)     =  x
  show (Arr n f)        =  par ("Arr" <+> show n <+> showf f)
  show (ArrLen a)       =  par ("ArrLen" <+> show a)
  show (ArrIx a i)      =  par ("ArrIx" <+> show a <+> show i)

showf :: (E a -> E b) -> String
showf f  =  par ("\\x->" <+> show (f (Variable "x")))

(<!>) :: (a -> b) -> a -> b
f <!> x  =  seq x (f x)

eval                  :: E a -> a
eval (LitI i)         =  i
eval (LitF x)         =  x
eval (LitB b)         =  b
eval (If c t e)       =  if eval c then eval t else eval e
eval (While c b i)    =  evalWhile (evalFun c) (evalFun b) (eval i)
eval (Pair a b)       =  (,) <!> eval a <!> eval b
eval (Fst p)          =  fst <!> eval p
eval (Snd p)          =  snd <!> eval p
eval (Prim1 _ f a)    =  f <!> eval a
eval (Prim2 _ f a b)  =  f <!> eval a <!> eval b
eval (Value a)        =  a
eval (Arr n ixf)      =  listArray (0,n') [ eval (ixf (LitI i)) | i <- [0..n'] ]
                         where n' = eval n - 1
eval (ArrLen a)       =  u - l + 1
                         where (l,u) = bounds (eval a)
eval (ArrIx a i)      =  eval a ! eval i

evalFun               :: (E a -> E b) -> a -> b
evalFun f x           =  (eval . f . Value) <!> x

evalWhile             :: (s -> Bool) -> (s -> s) -> s -> s
evalWhile c b i       =  if c i then evalWhile c b (b i) else i

class Syntactic a where
  type Internal a
  toE    ::  a -> E (Internal a)
  fromE  ::  E (Internal a) -> a

instance Syntactic (E a) where
  type Internal (E a)  =  a
  toE x                =  x        
  fromE x              =  x

true, false  :: E Bool
true         =  LitB True
false        =  LitB False

instance Num (E Int) where
  a + b          =  Prim2 "(+)" (+) a b 
  a - b          =  Prim2 "(-)" (-) a b 
  a * b          =  Prim2 "(*)" (*) a b
  fromInteger a  =  LitI (fromInteger a)
  abs a          =  Prim1 "abs" abs a
  signum a       =  Prim1 "signum" signum a

instance Num (E Float) where
  a + b          =  Prim2 "(+)" (+) a b 
  a - b          =  Prim2 "(-)" (-) a b 
  a * b          =  Prim2 "(*)" (*) a b
  fromInteger a  =  LitF (fromInteger a)
  abs a          =  Prim1 "abs" abs a
  signum a       =  Prim1 "signum" signum a

instance Fractional (E Float) where
  a / b           =  Prim2 "(/)" (/) a b
  fromRational a  =  LitF (fromRational a)

(.<.)        :: E Int -> E Int -> E Bool
a .<. b      =  Prim2 "(<)" (<) a b

(.==.)       :: (Syntactic a, Eq (Internal a)) => a -> a -> E Bool
a .==. b     =  Prim2 "(==)" (==) (toE a) (toE b)

modd         :: E Int -> E Int -> E Int
a `modd` b   =  Prim2 "mod" mod a b

minC         :: E Int -> E Int -> E Int
minC a b     =  (a .<. b) ? (a, b)

ifC          :: Syntactic a => E Bool -> a -> a -> a
ifC c t e    =  fromE (If c (toE t) (toE e))

(?)          :: Syntactic a => E Bool -> (a, a) -> a
c ? (t,e)    =  ifC c t e

while	     :: Syntactic s => (s -> E Bool) -> (s -> s) -> s -> s
while c b i  =  fromE (While (c . fromE) (toE . b . fromE) (toE i))

instance (Syntactic a, Syntactic b) => Syntactic (a,b) where
  type Internal (a,b)  =  (Internal a, Internal b)
  toE (a,b)         =  Pair (toE a) (toE b)
  fromE p           =  (fromE (Fst p), fromE (Snd p))

for        :: Syntactic a => E Int -> a -> (E Int -> a -> a) -> a
for n x0 b  =  snd (while (\(i,x) -> i.<.n) (\(i,x) -> (i+1, b i x)) (0,x0))

class Syntactic a => Undef a where
  undef :: a

instance Undef (E Bool) where
  undef = false

instance Undef (E Int) where
  undef = 0

instance Undef (E Float) where
  undef = 0

instance (Undef a, Undef b) => Undef (a,b) where
  undef = (undef, undef)

{-

instance (Ix a, Undef a, Undef b) => Undef (Array a b) where
  undef = array (undef,undef) [(undef,undef)]

-}

returnn :: a -> Opt_R a
returnn x  =  some_R x

(>>>=) :: (Undef b) => Opt_R a -> (a -> Opt_R b) -> Opt_R b
o >>>= g  =  Opt_R (def o ? (def (g (val o)), false))
                   (def o ? (val (g (val o)), undef))

data Opt_R a = Opt_R { def :: E Bool, val :: a }

instance Syntactic a => Syntactic (Opt_R a) where
  type Internal (Opt_R a)  =  (Bool, Internal a)
  fromE p                  =  Opt_R (Fst p) (fromE (Snd p))
  toE (Opt_R b x)          =  Pair b (toE x)

some_R :: a -> Opt_R a
some_R a  =  Opt_R true a

none_R :: Undef a => Opt_R a
none_R = Opt_R false undef

option_R :: Syntactic b => b -> (a -> b) -> Opt_R a -> b
option_R d f o  =  def o ? (f (val o), d)

newtype Opt a = O { unO :: forall b . Undef b => ((a -> Opt_R b) -> Opt_R b) }

instance Undef a => Syntactic (Opt a) where
  type Internal (Opt a) = (Bool, Internal a)
  fromE = lift . fromE
  toE   = toE . lower

instance Functor Opt where
  fmap f m  =  do x <- m; return (f x)

instance Applicative Opt where
  pure = return
  m <*> n  =  do f <- m; x <- n; return (f x)

instance Monad Opt where
  return x    = O (\g -> g x)
  m >>= k     = O (\g -> unO m (\x -> unO (k x) g))

lift          :: Opt_R a -> Opt a
lift o        =  O (\g -> Opt_R (def o ? (def (g (val o)), false))
                                (def o ? (val (g (val o)), undef)))

lower         :: Undef a => Opt a -> Opt_R a
lower m       =  unO m some_R

some          :: a -> Opt a
some a        =  lift (some_R a)

none          :: Undef a => Opt a
none          =  lift none_R

option        :: (Undef a, Undef b) => b -> (a -> b) -> Opt a -> b
option d f o  =  option_R d f (lower o) 

data Vector a where
  Indexed :: E Int -> (E Int -> a) -> Vector a

len     :: E (Array Int a) -> E Int
len a   =  ArrLen a

(!#)    :: Syntactic a => E (Array Int (Internal a)) -> E Int -> a
a !# i  =  fromE (ArrIx a i)

instance Syntactic a => Syntactic (Vector a) where
  type Internal (Vector a)  =  Array Int (Internal a)
  toE (Indexed n f)      =  Arr n (toE . f . fromE)
  fromE a                =  Indexed (len a) (\i -> a !# i)

instance Functor Vector where
  fmap f (Indexed n g)  =  Indexed n (f . g)

zipWithVec :: (Syntactic a, Syntactic b) =>
                 (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithVec f (Indexed m g) (Indexed n h)
  =  Indexed (minC m n) (\i -> f (g i) (h i))

sumVec :: (Syntactic a, Num a) => Vector a -> a
sumVec (Indexed n f) = for n 0 (\i s -> s + f i)

filterVec :: (Undef a) => (a -> E Bool) -> Vector a -> Vector (Opt a)
filterVec p  =  fmap (\x -> ifC (p x) (some x) none)

idVec :: E Int -> Vector (E Int)
idVec n  =  Indexed n id

-- Smart constructors for simplification

makeIf :: E Bool -> E a -> E a -> E a
makeIf (LitB True)  t e  =  t
makeIf (LitB False) t e  =  e
makeIf (If c t0 e0) t e  =  If c (makeIf t0 t e) (makeIf e0 t e)
makeIf c t e             =  If c t e

makeFst :: E (a,b) -> E a
makeFst (If c t e)  =  If c (makeFst t) (makeFst e)
makeFst (Pair a b)  =  a
makeFst p           =  Fst p

makeSnd :: E (a,b) -> E b
makeSnd (If c t e)  =  If c (makeSnd t) (makeSnd e)
makeSnd (Pair a b)  =  b
makeSnd p           =  Snd p

simplify :: E a -> E a
simplify (LitI i)         =  LitI i
simplify (LitF x)         =  LitF x
simplify (LitB b)         =  LitB b
simplify (While c b i)    =  While (simplifyf c) (simplifyf b) (simplify i)
simplify (If c t e)       =  makeIf (simplify c) (simplify t) (simplify e)
simplify (Pair a b)       =  Pair (simplify a) (simplify b)
simplify (Fst p)          =  makeFst (simplify p)
simplify (Snd p)          =  makeSnd (simplify p)
simplify (Prim1 s f a)    =  Prim1 s f (simplify a)
simplify (Prim2 s f a b)  =  Prim2 s f (simplify a) (simplify b)
simplify (Value a)        =  Value a
simplify (Arr n ixf)      =  Arr (simplify n) (simplifyf ixf)
simplify (ArrLen a)       =  ArrLen (simplify a)
simplify (ArrIx a i)      =  ArrIx (simplify a) (simplify i)

simplifyf :: (E a -> E b) -> E a -> E b
simplifyf f x  =  simplify (f x)


-- Examples

scalarProd :: (Syntactic a, Num a) => Vector a -> Vector a -> a
scalarProd a b = sumVec (zipWithVec (*) a b)

sqr :: Num a => a -> a
sqr x  =  x * x

power :: Int -> E Float -> E Float
power n x =
  if n < 0 then
    x .==. 0 ? (0, 1 / power (-n) x)
  else if n == 0 then
    1
  else if even n then
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x


power' :: Int -> E Float -> Opt (E Float)
power' n x =
  if n < 0 then
    (x .==. 0) ? (none, do y <- power' (-n) x; return (1/y))
  else if n == 0 then
    return 1
  else if even n then
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x*y)

power'' :: Int -> E Float -> E Float
power'' n x  =  option 0 (\y -> y) (power' n x)


{-
power 0 x  =  1
power n x  | n < 0           =  x .==. 0 ? (0, 1 / power (-n) x)
           | n `mod` 2 == 0  =  sqr (power (n `div` 2) x)
           | otherwise       =  x * power (n - 1) x

(/#) :: E Float -> E Float -> Opt (E Float)
x /# y  =  (y .==. 0) ? (none, some (x/y))

powerOpt 0 x  =  return 1
powerOpt n x | n < 0            =  do y <- powerOpt (-n) x
                                    1 /# y
           | n `mod` 2 == 0   =  do y <- powerOpt (n `div` 2) x
                                    return (sqr y)
           | otherwise        =  do y <- powerOpt (n - 1) x
                                    return (x * y)
-}

ex0 :: E Int
ex0 =  scalarProd (idVec 10) (fmap (* 2) (idVec 10))

check0 :: Bool
check0 =  eval ex0 == sum (zipWith (*) [0..9] (map (*2) [0..9]))

{-

The value of ex0 is:

  (Snd (While (\x-> ((<) (Fst x)
                         (If ((<) (LitI 10) (LitI 10))
                             (LitI 10)
                             (LitI 10))))
              (\x-> (Pair ((+) (Fst x) (LitI 1))
                          ((+) (Snd x) ((*) (Fst x) ((*) (Fst x) (LitI 2))))))
              (Pair (LitI 0) (LitI 0))))

-}

ex1 :: E Float
ex1 =  power (-6) 2

check1 :: Bool
check1 =  eval ex1 == 1/2^6

ex1z :: E Float
ex1z =  power (-1) 0

check1z :: Bool
check1z =  eval ex1z == 0

ex1o :: E Float
ex1o =  power'' (-6) 2

check1o :: Bool
check1o =  eval ex1o == 1/2^6

ex1oz :: E Float
ex1oz =  power'' (-1) 0

check1oz :: Bool
check1oz =  eval ex1oz == 0

{- 

The value of ex1 is:

  ((*) (LitI 42)
       ((*) ((*) (LitI 42)
                 ((*) ((*) (LitI 42) (LitI 1))
                      ((*) (LitI 42) (LitI 1))))
            ((*) (LitI 42)
                 ((*) ((*) (LitI 42) (LitI 1))
                      ((*) (LitI 42) (LitI 1))))))

-}

ex2 :: E Int
ex2 =  sumVec (fmap (\x -> option 0 id x) (filterVec (\x -> x `modd` 2 .==. 0) (idVec 10)))

check2 :: Bool
check2 =  eval ex2 == sum [ x | x <- [0..9], x `mod` 2 == 0 ]

{-

The value of ex2 is:

  (Snd (While
    (\x-> ((<) (Fst x) (LitI 10)))
    (\x-> (Pair
            ((+) (Fst x) (LitI 1))
            ((+) (Snd x)
              (If (Fst (If ((==) (mod (Fst x) (LitI 2)) (LitI 0))
                           (Pair (LitB True) (Fst x))
                           (Pair (LitB False) Undef)))
                  (Snd (If ((==) (mod (Fst x) (LitI 2)) (LitI 0))
                           (Pair (LitB True) (Fst x))
                           (Pair (LitB False) Undef)))
                  (LitI 0)))))
    (Pair (LitI 0) (LitI 0))))

After simplification, this becomes:

  (Snd (While
    (\x-> ((<) (Fst x) (LitI 10)))
    (\x-> (Pair
            ((+) (Fst x) (LitI 1))
            ((+) (Snd x)
              (If ((==) (mod (Fst x) (LitI 2)) (LitI 0))
                  (Fst x)
                  (LitI 0)))
    (Pair (LitI 0) (LitI 0))))

-}

main =
  check0 && check1 && check1z && check1o && check1oz && check2

