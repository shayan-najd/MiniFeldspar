module Expression.Feldspar.Mini where

import Data.Array

data Exp a where
  ConI  :: Integer -> Exp Integer
  ConB  :: Bool -> Exp Bool
  Cnd   :: Exp Bool -> Exp a -> Exp a -> Exp a
  Whl   :: (Exp s -> Exp Bool) -> (Exp s -> Exp s) -> Exp s -> Exp s
  Tpl   :: Exp a -> Exp b -> Exp (a , b)
  Fst   :: Exp (a , b) -> Exp a
  Snd   :: Exp (a , b) -> Exp b
  Ary   :: Exp Integer -> (Exp Integer -> Exp a) -> Exp (Array Integer a)
  Len   :: Exp (Array Integer a) -> Exp Integer
  Ind   :: Exp (Array Integer a) -> Exp Integer -> Exp a
  Let   :: Exp a -> ( a -> Exp b) -> Exp b
  Var   :: String -> Exp a 
  Prm1  :: String -> (a -> b) -> Exp a -> Exp b
  Prm2  :: String -> (a -> b -> c) -> Exp a -> Exp b -> Exp c
  Val   :: a -> Exp a
  Undef :: Exp a 
  
   
