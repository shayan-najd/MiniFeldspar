{-# LANGUAGE TemplateHaskell, GADTs #-}

module TemplateHaskell where

import Language.Haskell.TH.Syntax
import Data.Array 
  
type Qu a = Q (TExp a)

class FO t where {}
  
instance FO Int where {}
instance (FO a , FO b) => FO (a , b) where {}
  
while :: FO s => (s -> Bool) -> (s -> s) -> s -> s
while p f s = if p s then while p f (f s) else s
              
arr :: Int -> (Int -> a) -> Array Int a
arr n f = listArray (0 , n - 1) [f i | i <- [0 .. n - 1]]
          
arrLen :: Array Int a -> Int
arrLen a = let (0 , n) = bounds a in n + 1
 
forLoop :: FO s => Qu (Int -> s -> (Int -> s -> s) -> s)
forLoop = [|| \ n s0 b -> snd (while (\ (i , _s) -> i < n) 
                                     (\ (i , s ) -> (i + 1 , b i s))   
                                     (0 , s0)) ||]          

forLoop' :: FO s => Int -> s -> (Int -> s -> s) -> s
forLoop' n s0 b = snd (while (\ (i , _s) -> i < n) 
                             (\ (i , s ) -> (i + 1 , b i s))   
                             (0 , s0))


ex1 :: Qu Int          
ex1 = [|| $$forLoop 10 0 (\ i s -> i + s) ||]

ex1' :: Qu Int          
ex1' = [|| forLoop' 10 0 (\ i s -> i + s) ||]

          
data Vector a where 
  Indexed :: Int -> (Int -> a) -> Vector a           
  
instance Functor Vector where
  fmap f (Indexed n g) = Indexed n ( f . g )  

zipWithVec :: Qu ((a -> b -> c) -> Vector a -> Vector b -> Vector c)
zipWithVec = [|| \ f (Indexed m g) (Indexed n h) -> 
                 Indexed (min m n) (\ i -> f (g i) (h i)) ||]

zipWithVec' :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithVec' f (Indexed m g) (Indexed n h) = Indexed (min m n) (\ i -> f (g i) (h i))
             
sumVec ::(FO a , Num a) => Qu (Vector a -> a)             
sumVec = [|| \ (Indexed n f) -> $$forLoop n 0 (\ i s -> s + (f i)) ||]

sumVec' ::(FO a , Num a) => Vector a -> a
sumVec' (Indexed n f) = forLoop' n 0 (\ i s -> s + (f i))


scalarProd :: (FO a , Num a) => Qu (Vector a -> Vector a -> a) 
scalarProd = [|| \ a b -> $$sumVec ($$zipWithVec (*) a b) ||] 

scalarProd' :: (FO a , Num a) => Vector a -> Vector a -> a
scalarProd' a b = sumVec' (zipWithVec' (*) a b)

idVec :: Qu (Int -> Vector Int)
idVec = [|| \ n -> Indexed n (\ i -> i) ||]

idVec' :: Int -> Vector Int
idVec' n = Indexed n (\ i -> i)

ex2 :: Qu Int
ex2 = [|| $$scalarProd ($$idVec 10) (fmap (* 2) ($$idVec 10)) ||]

ex2' :: Qu Int
ex2' = [|| scalarProd' (idVec' 10) (fmap (* 2) (idVec' 10)) ||]

memorize :: Qu (Vector a -> Vector a)
memorize = [|| \ (Indexed n f) -> let a = arr n f in Indexed n (\ i -> a ! i) ||]  

memorize' :: Vector a -> Vector a
memorize' (Indexed n f) = let a = arr n f in Indexed n (\ i -> a ! i)

ex3 :: Qu Int
ex3 = [|| let vec = $$memorize ($$idVec 10)
          in  $$scalarProd vec (fmap (* 2) vec) ||]

ex3' :: Qu Int
ex3' = [|| let vec = memorize' (idVec' 10)
           in  scalarProd' vec (fmap (* 2) vec) ||]
