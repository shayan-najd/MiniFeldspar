{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Examples.Feldspar.Prelude.Mini where

import Expression.Feldspar.Mini

import Prelude hiding (Ord(..),Eq(..))
import qualified Prelude as P
import Data.Array

true , false :: Exp Bool
true  = ConB True
false = ConB False

ifC :: Exp Bool -> Exp a -> Exp a -> Exp a
ifC = Cnd 

(?) :: Exp Bool -> (Exp a, Exp a) -> Exp a
c ? ( t , e) = ifC c t e

while ::  (Exp s -> Exp Bool) -> (Exp s -> Exp s ) -> Exp s -> Exp s
while = Whl 

forLoop :: Exp Integer -> Exp s -> (Exp Integer -> Exp s -> Exp s ) -> Exp s
forLoop ln int step = Snd $ while (\ (Tpl i  _) -> i<ln )
                           (\ (Tpl i  s) -> Tpl (i+1) (step i s))
                           (Tpl (0 :: Exp Integer) int)  
                        
(==) :: P.Eq a => Exp a -> Exp a -> Exp Bool
(==) = Prm2 "==" (P.==)

compare :: P.Ord a => Exp a -> Exp a -> Exp Ordering
compare = Prm2 "compare" (P.compare)

(<) :: P.Ord a => Exp a -> Exp a -> Exp Bool
(<) = Prm2 "<" (P.<)

(>=) :: P.Ord a => Exp a -> Exp a -> Exp Bool
(>=) = Prm2 ">=" (P.>=)

(>) :: P.Ord a => Exp a -> Exp a -> Exp Bool
(>) = Prm2 ">" (P.>)

(<=) :: P.Ord a => Exp a -> Exp a -> Exp Bool
(<=) = Prm2 "<=" (P.<=)

max :: P.Ord a => Exp a -> Exp a -> Exp a
max = Prm2 "max" (P.max)

min :: P.Ord a => Exp a -> Exp a -> Exp a
min = Prm2 "min" (P.min)

instance Num a => Num (Exp a) where  
  (+) = Prm2 "+" (+)
  (-) = Prm2 "-" (-)
  (*) = Prm2 "-" (-)
  abs = Prm1 "abs" abs
  signum = Prm1 "signum" signum
  fromInteger = Val . fromInteger

data Option a = Option {isSome :: Exp Bool , fromSome :: a}

  
undef :: Exp a
undef = Undef  

some :: a -> Option a
some a = Option true a

none :: Option (Exp a)
none = Option false undef

option :: Exp b -> (Exp a -> Exp b) -> Option (Exp a) -> Exp b
option noneCase someCase opt = ifC (isSome opt)
                               (someCase (fromSome opt ))
                               noneCase
                    
instance Functor Option where
  fmap f (Option b a) = Option b (f a)
  
instance Monad Option where
  return a = some a
  opt >>= k = b {isSome = isSome opt ? (isSome b , false ) }
    where b = k (fromSome opt)                               
          
          
divO :: Exp Integer -> Exp Integer -> Option (Exp Integer )
divO f1 f2 = let
  Val x = f1 
  Val y = f2
  in if y P.== 0 
     then none
     else some $ ConI $  x `div` y
          
divTest :: Exp Integer -> Exp Integer -> Exp Integer -> Option (Exp Integer )
divTest a b c = do r1 <- divO a b
                   r2 <- divO a c
                   return (r1 + r2)          
                   
                   
len :: Exp (Array Integer a) -> Exp Integer
len arr = Len arr

(<!>) ::  Exp (Array Integer a) -> Exp Integer -> Exp a
(<!>) = Ind                 


data Vector a where
  Indexed :: Exp Integer -> (Exp Integer -> a) -> Vector a
  
zipWithVec :: (Exp a -> Exp  b -> c) -> Vector (Exp a) 
              -> Vector (Exp b) -> Vector c
zipWithVec f (Indexed l1 ixf1 ) ( Indexed l2 ixf2 ) = 
  Indexed (min l1 l2 ) (\ ix -> f (ixf1 ix) (ixf2 ix))

sumVec :: (Num a) => Vector (Exp a) -> (Exp a)
sumVec (Indexed l ixf) = forLoop l 0 (\ ix s -> s + ixf ix )

instance Functor Vector where
  fmap f (Indexed l ixf) = Indexed l ( f . ixf )  
  
scalarProd :: Num a => Vector (Exp a) -> Vector (Exp a) -> Exp a
scalarProd a b = sumVec (zipWithVec (*) a b)

memorize :: Vector (Exp a) -> Vector (Exp a)
memorize (Indexed l ixf) = Indexed l (\ n -> Ary l ixf  <!> n)

 