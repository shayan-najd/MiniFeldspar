{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs , StandaloneDeriving #-}
module Data.Vector where

import Data.Foldable
import Data.Monoid
import Data.Nat.GADT

infixr 5 :::
data Vec n a where   
  Nil   :: Vec Zro a
  (:::) :: a -> Vec n a -> Vec (Suc n) a
           
deriving instance Eq a => Eq (Vec n a)

instance Functor (Vec n) where
  fmap _ Nil        = Nil
  fmap f (x ::: xs) = f x ::: fmap f xs

instance Foldable (Vec n) where 
  foldMap _ Nil        = mempty
  foldMap f (x ::: xs) = f x `mappend` foldMap f xs
