{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs , StandaloneDeriving, DataKinds #-}
module Data.Vector where

import Data.Foldable
import Data.Monoid
import Singleton.Nat
import qualified Data.Nat as N

infixr 5 :::
data Vec n a where   
  Nil   :: Vec 'N.Zro a
  (:::) :: a -> Vec n a -> Vec ('N.Suc n) a
           
deriving instance Eq a => Eq (Vec n a)

instance Functor (Vec n) where
  fmap _ Nil        = Nil
  fmap f (x ::: xs) = f x ::: fmap f xs

instance Foldable (Vec n) where 
  foldMap _ Nil        = mempty
  foldMap f (x ::: xs) = f x `mappend` foldMap f xs

len :: Vec n a -> Nat n
len Nil        = Zro
len (_ ::: xs) = Suc (len xs)