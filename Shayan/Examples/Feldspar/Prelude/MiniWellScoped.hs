{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, Rank2Types, ImpredicativeTypes, NoMonomorphismRestriction #-}
module Examples.Feldspar.Prelude.MiniWellScoped where

import Expression.Feldspar.MiniWellScoped

import Prelude hiding (Ord(..),Eq(..))
import qualified Prelude as P
import Data.Array
import Environment.GADT 
import Variable.GADT

type EnvCon r =   
             (forall a. P.Eq a => a -> a -> Bool,
              (forall a. P.Ord a => a -> a -> Ordering,
               (forall a. P.Ord a => a -> a -> Bool,
                (forall a. P.Ord a => a -> a -> Bool,
                 (forall a. P.Ord a => a -> a -> Bool,
                  (forall a. P.Ord a => a -> a -> Bool,
                   (forall a. P.Ord a => a -> a -> a ,
                    (forall a. P.Ord a => a -> a -> a,
                     (forall a. P.Num a => a -> a -> a,
                      (forall a. P.Num a => a -> a -> a,
                       (forall a. P.Num a => a -> a -> a, 
                        (forall a. P.Num a => a -> a, 
                         (forall a. P.Num a => a -> a, r)))))))))))))
             
env :: r -> EnvCon r
env r = ((P.==),
         (P.compare,
          ((P.<),
           ((P.>=),
            ((P.>),
             ((P.<=),
              (P.max,
               (P.min,
                ((+),
                 ((-),
                  ((*),
                   (abs,
                    (signum,r)))))))))))))

prm1 :: Var (EnvCon r) (ta -> tb) -> Exp (EnvCon r) ta -> Exp (EnvCon r) tb
prm1 i = Prm1 i

prm2 :: Var (EnvCon r) (ta -> tb -> tc) -> Exp (EnvCon r) ta -> 
        Exp (EnvCon r) tb -> Exp (EnvCon r) tc
prm2 i = Prm2 i

true , false :: Exp r Bool
true  = ConB True
false = ConB False

ifC :: Exp r Bool -> Exp r a -> Exp r a -> Exp r a
ifC = Cnd 

(?) :: Exp r Bool -> (Exp r a, Exp r a) -> Exp r a
c ? ( t , e) = ifC c t e

while ::  (Exp r s -> Exp r Bool) -> (Exp r s -> Exp r s ) -> Exp r s -> Exp r s
while = Whl 

-- uncomment to see the error
-- (==) :: P.Eq a => Exp (EnvCon r) a -> Exp (EnvCon r) a -> Exp (EnvCon r) Bool
-- (==) = Prm2 Zro (P.==)

{-
compare :: P.Ord a => Exp r a -> Exp r a -> Exp r Ordering
compare = prm2 (Suc Zro)  

(<) :: P.Ord a => Exp (EnvCon r) a -> Exp (EnvCon r) a -> Exp (EnvCon r) Bool
(<) = prm2 (Suc $ Suc Zro) 

(>=) :: P.Ord a => Exp r a -> Exp r a -> Exp r Bool
(>=) = prm2 (Suc $ Suc $ Suc Zro)

(>) :: P.Ord a => Exp r a -> Exp r a -> Exp r Bool
(>) = prm2 (Suc $ Suc $ Suc $ Suc Zro)

(<=) :: P.Ord a => Exp r a -> Exp r a -> Exp r Bool
(<=) = prm2 (Suc $ Suc $ Suc $ Suc $ Suc Zro) 

max :: P.Ord a => Exp r a -> Exp r a -> Exp r a
max = prm2  (Suc $ Suc $ Suc $ Suc $ Suc $ Suc Zro)  

min :: P.Ord a => Exp r a -> Exp r a -> Exp r a
min = prm2 (Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc Zro) 

instance Num a => Num (Exp r a) where  
  (+) = prm2 (Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc Zro)  
  (-) = prm2 (Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc Zro)  
  (*) = prm2 (Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc Zro)  
  abs = prm1 (Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc 
              $ Suc Zro )  
  signum = prm1 (Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc $ Suc 
                 $ Suc $ Suc Zro )   
  fromInteger = Val . fromInteger

data Option r a = Option {isSome :: Exp r Bool , fromSome :: a}

  
undef :: Exp r a
undef = Undef  

some :: a -> Option r a
some a = Option true a

none :: Option r (Exp r a)
none = Option false undef

option :: Exp r b -> (Exp r a -> Exp r b) -> Option r (Exp r a) -> Exp r b
option noneCase someCase opt = ifC (isSome opt)
                               (someCase (fromSome opt ))
                               noneCase
                    
instance Functor (Option r) where
  fmap f (Option b a) = Option b (f a)
  
instance Monad (Option r) where
  return a = some a
  opt >>= k = b {isSome = isSome opt ? (isSome b , false ) }
    where b = k (fromSome opt)                               
          
          
divO :: Exp r Integer -> Exp r Integer -> Option r (Exp r Integer )
divO f1 f2 = let
  Val x = f1 
  Val y = f2
  in if y P.== 0 
     then none
     else some $ ConI $  x `div` y
          
divTest :: Exp r Integer -> Exp r Integer -> Exp r Integer -> Option r (Exp r Integer )
divTest a b c = do r1 <- divO a b
                   r2 <- divO a c
                   return (r1 + r2)          
                   
                   
len :: Exp r (Array Integer a) -> Exp r Integer
len arr = Len arr

(<!>) ::  Exp r (Array Integer a) -> Exp r Integer -> Exp r a
(<!>) = Ind                 


data Vector r a where
  Indexed :: Exp r Integer -> (Exp r Integer -> a) -> Vector r a
  
zipWithVec :: (Exp r a -> Exp r  b -> c) -> Vector r (Exp r a) 
              -> Vector r (Exp r b) -> Vector r c
zipWithVec f (Indexed l1 ixf1 ) ( Indexed l2 ixf2 ) = 
  Indexed (min l1 l2 ) (\ ix -> f (ixf1 ix) (ixf2 ix))

sumVec :: (Num a) => Vector r (Exp r a) -> (Exp r a)
sumVec (Indexed l ixf) = forLoop l 0 (\ ix s -> s + ixf ix )

instance Functor (Vector r) where
  fmap f (Indexed l ixf) = Indexed l ( f . ixf )  
  
scalarProd :: Num a => Vector r (Exp r a) -> Vector r (Exp r a) -> Exp r a
scalarProd a b = sumVec (zipWithVec (*) a b)

memorize :: Vector r (Exp r a) -> Vector r (Exp r a)
memorize (Indexed l ixf) = Indexed l (\ n -> Ary l ixf  <!> n)

forLoop :: Exp (EnvCon r) Integer -> Exp (EnvCon r) s -> 
           (Exp (EnvCon r) Integer -> Exp (EnvCon r) s -> Exp (EnvCon r) s ) -> 
           Exp (EnvCon r) s
forLoop ln int step = Snd $ while (\ (Tpl i  _) -> i < ln )
                           (\ (Tpl i  s) -> Tpl (i+1) (step i s))
                           (Tpl (0 :: Exp (EnvCon r) Integer) int) 

-}