{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs,TypeFamilies,FlexibleInstances #-}

import Prelude hiding (Ord(..),Eq(..))
import qualified Prelude as P
import Data.Array

data FunC a where
  LitI :: Int -> FunC Int
  LitB :: Bool -> FunC Bool
  If   :: FunC Bool -> FunC a -> FunC a -> FunC a
  While :: (FunC s -> FunC Bool) -> (FunC s -> FunC s ) -> FunC s -> FunC s
  Pair :: FunC a -> FunC b -> FunC (a , b)
  Fst :: FunC (a , b) -> FunC a
  Snd :: FunC (a , b) -> FunC b
  Prim1 :: String -> (a -> b) -> FunC a -> FunC b
  Prim2 :: String -> (a -> b -> c) -> FunC a -> FunC b -> FunC c
  Value :: a -> FunC a
  Variable :: String -> FunC a
  Undef :: FunC a 
  Arr :: FunC Int -> (FunC Int -> FunC a) -> FunC (Array Int a)
  ArrLen :: FunC (Array Int a) -> FunC Int
  ArrIx :: FunC (Array Int a) -> FunC Int -> FunC a
  Let   :: FunC a -> ( a -> FunC b) -> FunC b
  
  
eval :: FunC a -> a
eval (LitI i )= i
eval (LitB b) = b
eval (While c b i ) = head $ dropWhile ( eval . c . Value) $
                      iterate ( eval . b . Value) $ eval i
eval (If c t e) = if eval c then eval t else eval e
eval (Pair a b) = (eval a , eval b)
eval (Fst p) = fst ( eval p)
eval (Snd p) = snd ( eval p)
eval (Prim1 _ f a) = f ( eval a)
eval (Prim2 _ f a b) = f ( eval a) ( eval b)
eval (Value a) = a
eval Undef   = undefined
eval (Arr l ixf) = listArray (0 ,lm1) [eval $ ixf $ Value i 
                                      | i <- [0 .. lm1]]
  where lm1 = eval l - 1
eval (ArrLen a) = (1 +) $ uncurry (flip (-)) $ bounds $ eval a
eval (ArrIx a i) = eval a ! eval i
eval (Let y f)   =  let x = eval y 
                    in  eval (f x)
eval (Variable _) = undefined                        

true , false :: FunC Bool
true  = LitB True
false = LitB False

ifC :: FunC Bool -> FunC a -> FunC a -> FunC a
ifC = If 

(?) :: FunC Bool -> (FunC a, FunC a) -> FunC a
c ? ( t , e) = ifC c t e

while ::  (FunC s -> FunC Bool) -> (FunC s -> FunC s ) -> FunC s -> FunC s
while = While 

forLoop :: FunC Int -> FunC s -> (FunC Int -> FunC s -> FunC s ) -> FunC s
forLoop ln int step = Snd $ while (\ (Pair i  _) -> i<ln )
                           (\ (Pair i  s) -> Pair (i+1) (step i s))
                           (Pair (0 :: FunC Int) int)  
                        
(==) :: P.Eq a => FunC a -> FunC a -> FunC Bool
(==) = Prim2 "==" (P.==)

compare :: P.Ord a => FunC a -> FunC a -> FunC Ordering
compare = Prim2 "compare" (P.compare)

(<) :: P.Ord a => FunC a -> FunC a -> FunC Bool
(<) = Prim2 "<" (P.<)

(>=) :: P.Ord a => FunC a -> FunC a -> FunC Bool
(>=) = Prim2 ">=" (P.>=)

(>) :: P.Ord a => FunC a -> FunC a -> FunC Bool
(>) = Prim2 ">" (P.>)

(<=) :: P.Ord a => FunC a -> FunC a -> FunC Bool
(<=) = Prim2 "<=" (P.<=)

max :: P.Ord a => FunC a -> FunC a -> FunC a
max = Prim2 "max" (P.max)

min :: P.Ord a => FunC a -> FunC a -> FunC a
min = Prim2 "min" (P.min)

instance Num a => Num (FunC a) where  
  (+) = Prim2 "+" (+)
  (-) = Prim2 "-" (-)
  (*) = Prim2 "-" (-)
  abs = Prim1 "abs" abs
  signum = Prim1 "signum" signum
  fromInteger = Value . fromInteger

data Option a = Option {isSome :: FunC Bool , fromSome :: a}

  
undef :: FunC a
undef = Undef  

some :: a -> Option a
some a = Option true a

none :: Option (FunC a)
none = Option false undef

option :: FunC b -> (FunC a -> FunC b) -> Option (FunC a) -> FunC b
option noneCase someCase opt = ifC (isSome opt)
                               (someCase (fromSome opt ))
                               noneCase
                    
instance Functor Option where
  fmap f (Option b a) = Option b (f a)
  
instance Monad Option where
  return a = some a
  opt >>= k = b {isSome = isSome opt ? (isSome b , false ) }
    where b = k (fromSome opt)                               
          
          
divO :: FunC Int -> FunC Int -> Option (FunC Int )
divO f1 f2 = let
  Value x = f1 
  Value y = f2
  in if y P.== 0 
     then none
     else some $ LitI $  x `div` y
          
divTest :: FunC Int -> FunC Int -> FunC Int -> Option (FunC Int )
divTest a b c = do r1 <- divO a b
                   r2 <- divO a c
                   return (r1 + r2)          
                   
                   
len :: FunC (Array Int a) -> FunC Int
len arr = ArrLen arr

(<!>) ::  FunC (Array Int a) -> FunC Int -> FunC a
(<!>) = ArrIx                 


data Vector a where
  Indexed :: FunC Int -> (FunC Int -> a) -> Vector a
  
zipWithVec :: (FunC a -> FunC  b -> c) -> Vector (FunC a) 
              -> Vector (FunC b) -> Vector c
zipWithVec f (Indexed l1 ixf1 ) ( Indexed l2 ixf2 ) = 
  Indexed (min l1 l2 ) (\ ix -> f (ixf1 ix) (ixf2 ix))

sumVec :: (Num a) => Vector (FunC a) -> (FunC a)
sumVec (Indexed l ixf) = forLoop l 0 (\ ix s -> s + ixf ix )

instance Functor Vector where
  fmap f (Indexed l ixf) = Indexed l ( f . ixf )  
  
scalarProd :: Num a => Vector (FunC a) -> Vector (FunC a) -> FunC a
scalarProd a b = sumVec (zipWithVec (*) a b)

memorize :: Vector (FunC a) -> Vector (FunC a)
memorize (Indexed l ixf) = Indexed l (\ n -> Arr l ixf  <!> n)

 