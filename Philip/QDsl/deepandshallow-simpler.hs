{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances #-}

-- This was an attempt to simplify nested conditionals by passing in the outer condition
-- Giving up, this is too hard (because of constraints imposed by GADTs).
-- File currently broken.
-- Philip Wadler, 7 Oct 2014

module DeepAndShallow where
import Data.Array

-- Combining Deep and Shallow Embedding
-- Philip Wadler, 30 Apr 2014

type Id = String

data FunC a where
  LitI      :: Int -> FunC Int
  LitF      :: Float -> FunC Float
  LitB      :: Bool -> FunC Bool
  If        :: FunC Bool -> FunC a -> FunC a -> FunC a
  While     :: (FunC s -> FunC Bool) -> (FunC s -> FunC s) -> FunC s -> FunC s
  Pair      :: FunC a -> FunC b -> FunC (a , b)
  Fst       :: FunC (a , b) -> FunC a
  Snd       :: FunC (a , b) -> FunC b
  Prim1     :: Id -> (a -> b) -> FunC a -> FunC b
  Prim2     :: Id -> (a -> b -> c) -> FunC a -> FunC b -> FunC c
  Value     :: a -> FunC a
  Variable  :: Id -> FunC a
  Undef     :: FunC a
  Arr       :: FunC Int -> (FunC Int -> FunC a) -> FunC (Array Int a)
  ArrLen    :: FunC (Array Int a) -> FunC Int
  ArrIx     :: FunC (Array Int a) -> FunC Int -> FunC a

data FunC' where
  LitI'      :: Int -> FunC'
  LitF'      :: Float -> FunC'
  LitB'      :: Bool -> FunC'
  If'        :: FunC' -> FunC' -> FunC' -> FunC'
  While'     :: Id -> FunC' -> FunC' -> FunC' -> FunC'
  Pair'      :: FunC' -> FunC' -> FunC'
  Fst'       :: FunC' -> FunC'
  Snd'       :: FunC' -> FunC'
  Prim1'     :: Id -> FunC' -> FunC'
  Prim2'     :: Id -> FunC' -> FunC' -> FunC'
  Variable'  :: String -> FunC'
  Undef'     :: FunC'
  Arr'       :: FunC' -> Id -> FunC' -> FunC'
  ArrLen'    :: FunC' -> FunC'
  ArrIx'     :: FunC' -> FunC' -> FunC'
  deriving Eq

erase :: FunC a -> FunC'
erase (LitI i)         =  LitI' i
erase (LitF x)         =  LitF' x
erase (LitB b)         =  LitB' b
erase (If c t e)       =  If' (erase c) (erase t) (erase e)
erase (While c b i)    =  While' x (erase (c x)) (erase (b x)) (erase i)
                       where x = Variable "x"
erase (Pair a b)       =  Pair' (erase a) (erase b)
erase (Fst p)          =  Fst' (erase p)
erase (Snd p)          =  Snd' (erase p)
erase (Prim1 s f a)    =  Prim1' s (erase a)
erase (Prim2 s f a b)  =  Prim2' s (erase a) (erase b)
erase (Value a)        =  GiveUp
erase Undef            =  Undef'
erase (Arr n ixf)      =  GiveUp
erase (ArrLen a)       =  GiveUp
erase (ArrIx a i)      =  GiveUp

instance Eq (FunC a) where
  a == b  =  erase a == erase b


par     :: String -> String
par s   =  "(" ++ s ++ ")"

(<+>)    :: String -> String -> String
s <+> t  =  s ++ " " ++ t

instance Show (FunC a) where
  show (LitI i)	        =  par ("LitI" <+> show i)
  show (LitF x)         =  par ("LitF" <+> show x)
  show (LitB b)	        =  par ("LitB" <+> show b)
  show (While c b i)    =  par ("While" <+> showf c <+> showf b <+> show i)
  show (If c t e)       =  par ("If" <+> show c <+> show t <+> show e)
  show (Pair a b)       =  par ("Pair" <+> show a <+> show b)
  show (Fst p)          =  par ("Fst" <+> show p)
  show (Snd p)          =  par ("Snd" <+> show p)
  show (Prim1 s f a)    =  par (s <+> show a)
  show (Prim2 s f a b)  =  par (s <+> show a <+> show b)
  show (Variable x)     =  x
  show Undef            =  "Undef"
  show (Arr n f)        =  par ("Arr" <+> show n <+> showf f)
  show (ArrLen a)       =  par ("ArrLen" <+> show a)
  show (ArrIx a i)      =  par ("ArrIx" <+> show a <+> show i)

showf :: (FunC a -> FunC b) -> String
showf f  =  par ("\\x->" <+> show (f (Variable "x")))

(<#>) :: (a -> b) -> a -> b
f <#> x  =  seq x (f x)

eval                  :: FunC a -> a
eval (LitI i)         =  i
eval (LitF x)         =  x
eval (LitB b)         =  b
eval (While c b i)    =  ewhile <#> evalf c <#> evalf b <#> eval i
eval (If c t e)       =  if eval c then eval t else eval e
eval (Pair a b)       =  (,) <#> eval a <#> eval b
eval (Fst p)          =  fst <#> eval p
eval (Snd p)          =  snd <#> eval p
eval (Prim1 _ f a)    =  f <#> eval a
eval (Prim2 _ f a b)  =  f <#> eval a <#> eval b
eval (Value a)        =  a
eval Undef            =  undefined
eval (Arr n ixf)      =  listArray (0,n') [ eval (ixf (LitI i)) | i <- [0..n'] ]
                         where n' = eval n - 1
eval (ArrLen a)       =  u - l + 1
                         where (l,u) = bounds (eval a)
eval (ArrIx a i)      =  eval a ! eval i

evalf                 :: (FunC a -> FunC b) -> a -> b
evalf f x             =  eval (f (Value x))

ewhile                :: (s -> Bool) -> (s -> s) -> s -> s
ewhile c b i          =  if c i then ewhile c b (b i) else i

class Syntactic a where
  type Internal a
  toFunC    ::  a -> FunC (Internal a)
  fromFunC  ::  FunC (Internal a) -> a

funC   :: (Syntactic a, Syntactic b) => (a -> b) -> FunC (Internal a) -> FunC (Internal b)
funC f =  toFunC . f . fromFunC

instance Syntactic (FunC a) where
  type Internal (FunC a)  =  a
  toFunC x                =  x        
  fromFunC x              =  x

true, false  :: FunC Bool
true         =  LitB True
false        =  LitB False

instance Num (FunC Int) where
  a + b          =  Prim2 "(+)" (+) a b 
  a - b          =  Prim2 "(-)" (-) a b 
  a * b          =  Prim2 "(*)" (*) a b
  fromInteger a  =  LitI (fromInteger a)
  abs a          =  Prim1 "abs" abs a
  signum a       =  Prim1 "signum" signum a

instance Num (FunC Float) where
  a + b          =  Prim2 "(+)" (+) a b 
  a - b          =  Prim2 "(-)" (-) a b 
  a * b          =  Prim2 "(*)" (*) a b
  fromInteger a  =  LitF (fromInteger a)
  abs a          =  Prim1 "abs" abs a
  signum a       =  Prim1 "signum" signum a

instance Fractional (FunC Float) where
  a / b           =  Prim2 "(/)" (/) a b
  fromRational a  =  LitF (fromRational a)

(%#)         :: FunC Int -> FunC Int -> FunC Int
a %# b       =  Prim2 "mod" mod a b

(<#)         :: FunC Int -> FunC Int -> FunC Bool
a <# b       =  Prim2 "(<)" (<) a b

(==#)        :: Eq a => FunC a -> FunC a -> FunC Bool
a ==# b      =  Prim2 "(==)" (==) a b

minC         :: FunC Int -> FunC Int -> FunC Int
minC a b     =  If (a <# b) a b

ifC          :: Syntactic a => FunC Bool -> a -> a -> a
ifC c t e    =  fromFunC (If c (toFunC t) (toFunC e))

(?)          :: Syntactic a => FunC Bool -> (a, a) -> a
c ? (t,e)    =  ifC c t e

while	     :: Syntactic s => (s -> FunC Bool) -> (s -> s) -> s -> s
while c b i  =  fromFunC (While (funC c) (funC b) (toFunC i))

instance (Syntactic a, Syntactic b) => Syntactic (a,b) where
  type Internal (a,b)  =  (Internal a, Internal b)
  toFunC (a,b)         =  Pair (toFunC a) (toFunC b)
  fromFunC p           =  (fromFunC (Fst p), fromFunC (Snd p))

forLoop        :: Syntactic s => FunC Int -> s -> (FunC Int -> s -> s) -> s
forLoop n s b  =  snd (while (\(i,s) -> i<#n) (\(i,s) -> (i+1, b i s)) (0,s))

data Option a = Option { isSome :: FunC Bool, fromSome :: a }
  deriving Show

instance Syntactic a => Syntactic (Option a) where
  type Internal (Option a)  =  (Bool, Internal a)
  -- changed from paper, by adding calls to fromFunC and toFunC.
  fromFunC m                =  Option (Fst m) (fromFunC (Snd m))
  toFunC (Option b a)       =  Pair b (toFunC a)

instance Monad Option where
  return a  =  Option { isSome = true, fromSome = a }
  m >>= k  =   n { isSome = isSome m ? (isSome n, false) }
    where  n = k (fromSome m)

undef    :: Syntactic a => a
undef    =  fromFunC Undef

some     :: a -> Option a
some a   =  Option true a

none     :: Syntactic a => Option a
none     =  Option false undef

option   :: (Syntactic a, Syntactic b) => b -> (a -> b) -> Option a -> b
option noneCase someCase opt =
  ifC (isSome opt) (someCase (fromSome opt)) noneCase

data Vector a where
  Indexed :: FunC Int -> (FunC Int -> a) -> Vector a

len     :: FunC (Array Int a) -> FunC Int
len a   =  ArrLen a

(!#)    :: Syntactic a => FunC (Array Int (Internal a)) -> FunC Int -> a
a !# i  =  fromFunC (ArrIx a i)

instance Syntactic a => Syntactic (Vector a) where
  type Internal (Vector a)  =  Array Int (Internal a)
  toFunC (Indexed n f)      =  Arr n (funC f)
  fromFunC a                =  Indexed (len a) (\i -> a !# i)

instance Functor Vector where
  fmap f (Indexed n g)  =  Indexed n (f . g)

zipWithVec :: (Syntactic a, Syntactic b) =>
                 (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithVec f (Indexed m g) (Indexed n h)
  =  Indexed (minC m n) (\i -> f (g i) (h i))

sumVec :: (Syntactic a, Num a) => Vector a -> a
sumVec (Indexed n f) = forLoop n 0 (\i s -> s + f i)

filterVec :: Syntactic a => (a -> FunC Bool) -> Vector a -> Vector (Option a)
filterVec p  =  fmap (\x -> ifC (p x) (some x) none)

idVec :: FunC Int -> Vector (FunC Int)
idVec n  =  Indexed n id

-- Smart constructors for simplification

makeIf :: [(FunC Bool,Bool)] -> FunC Bool -> FunC a -> FunC a -> FunC a
makeIf cs (LitB b)  t e     =  if b then t else e
makeIf cs (If c t0 e0) t e  =  makeIf cs c
                                         (makeIf ((c,True) :cs) t0 t e)
                                         (makeIf ((c,False):cs) e0 t e)
makeIf cs c t e             =  case lookup c cs of
                                 Nothing  -> If c t e
                                 Just b   -> if b then t else e

makeFst :: [(FunC Bool,Bool)] -> FunC (a,b) -> FunC a
makeFst cs (If c t e)  =  makeIf cs c (makeFst cs t) (makeFst cs e)
makeFst cs (Pair a b)  =  a
makeFst cs p           =  Fst p

makeSnd :: [(FunC Bool,Bool)] -> FunC (a,b) -> FunC b
makeSnd cs (If c t e)  =  makeIf cs c (makeSnd cs t) (makeSnd cs e)
makeSnd cs (Pair a b)  =  b
makeSnd cs p           =  Snd p

simp :: [(FunC Bool,Bool)] -> FunC a -> FunC a
simp cs (LitI i)         =  LitI i
simp cs (LitF x)         =  LitF x
simp cs (LitB b)         =  LitB b
simp cs (While c b i)    =  While (simpf cs c) (simpf cs b) (simp cs i)
simp cs (If c t e)       =  makeIf cs (simpb cs (simp cs c))
                                      (simp ((c,True) :cs) t)
                                      (simp ((c,False):cs) e)
simp cs (Pair a b)       =  Pair (simp cs a) (simp cs b)
simp cs (Fst p)          =  makeFst cs (simp cs p)
simp cs (Snd p)          =  makeSnd cs (simp cs p)
simp cs (Prim1 s f a)    =  Prim1 s f (simp cs a)
simp cs (Prim2 s f a b)  =  Prim2 s f (simp cs a) (simp cs b)
simp cs (Value a)        =  Value a
simp cs (Variable x)     =  Variable x
simp cs Undef            =  Undef
simp cs (Arr n ixf)      =  Arr (simp cs n) (simpf cs ixf)
simp cs (ArrLen a)       =  ArrLen (simp cs a)
simp cs (ArrIx a i)      =  ArrIx (simp cs a) (simp cs i)

simpb :: [(FunC Bool,Bool)] -> FunC Bool -> FunC Bool
simpb cs c  =  case lookup c cs of
                 Nothing -> c
                 Just b  -> LitB b

simpf :: [(FunC Bool,Bool)] -> (FunC a -> FunC b) -> FunC a -> FunC b
simpf cs f x  =  simp cs (f x)

simplify :: FunC a -> FunC a
simplify =  simp []


-- Examples

scalarProd :: (Syntactic a, Num a) => Vector a -> Vector a -> a
scalarProd a b = sumVec (zipWithVec (*) a b)

sqr :: Num a => a -> a
sqr x  =  x * x

power :: Int -> FunC Float -> FunC Float
power 0 x  =  1
power n x  | n < 0           =  x ==# 0 ? (0, 1 / power (-n) x)
           | n `mod` 2 == 0  =  sqr (power (n `div` 2) x)
           | otherwise       =  x * power (n - 1) x


(/#) :: FunC Float -> FunC Float -> Option (FunC Float)
x /# y  =  (y ==# 0) ? (none, some (x/y))

powerO :: Int -> FunC Float -> Option (FunC Float)
powerO 0 x  =  return 1
powerO n x | n < 0            =  x ==# 0 ? 
                                    (none, 
                                      do y <- powerO (-n) x
                                         return (1 / y))
           | n `mod` 2 == 0   =  do y <- powerO (n `div` 2) x
                                    return (sqr y)
           | otherwise        =  do y <- powerO (n - 1) x
                                    return (x * y)

ex0 :: FunC Int
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

ex1 :: FunC Float
ex1 =  power (-6) 2

check1 :: Bool
check1 =  eval ex1 == 1/2^6

{- 

The value of ex1 is:

  ((*) ((*) (LitF 2.0)
            ((*) ((*) (LitF 2.0) (LitF 1.0))
                 ((*) (LitF 2.0) (LitF 1.0))))
       ((*) (LitF 2.0)
            ((*) ((*) (LitF 2.0) (LitF 1.0))
                 ((*) (LitF 2.0) (LitF 1.0)))))

-}

ex2 :: FunC Int
ex2 =  sumVec (fmap (\x -> option 0 id x) (filterVec (\y -> (y %# 2) ==# 0) (idVec 10)))

ans2 :: Int
ans2 =  sum [ x | x <- [0..9], x `mod` 2 == 0 ]

check2 :: Bool
check2 =  eval ex2 == ans2  -- undefined!

ex2s :: FunC Int
ex2s =  simplify ex2

check2s :: Bool
check2s =  eval ex2s == ans2  -- ok

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

ex1z :: FunC Float
ex1z =  power (-1) 0

check1z :: Bool
check1z =  eval ex1z == 0

ex1o :: FunC Float
ex1o =  option 0 id (powerO (-6) 2)

check1o :: Bool
check1o =  eval ex1o == 1/2^6

ex1oz :: FunC Float
ex1oz =  option 0 id (powerO (-1) 0)

check1oz :: Bool
check1oz =  eval ex1oz == 0  -- undefined!

ex1ozs :: FunC Float
ex1ozs =  simplify ex1oz

check1ozs :: Bool
check1ozs =  eval ex1ozs == 0  -- ok!

main :: Bool
main = check0 && check1 && check2s && check1z && check1o && check1ozs