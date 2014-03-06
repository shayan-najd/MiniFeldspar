module Data.Vector where

import Data.Foldable
import Data.Monoid
import Data.Traversable 
import Singleton.Nat
import qualified Data.Nat as N
import qualified Data.Fin as F
import Control.Applicative

infixr 5 :::
data Vec :: N.Nat -> * -> * where   
  Nil   :: Vec N.Zro t
  (:::) :: t -> Vec n t -> Vec (N.Suc n) t
           
deriving instance Eq a => Eq (Vec n a)

instance Functor (Vec n) where
  fmap _ Nil        = Nil
  fmap f (x ::: xs) = f x ::: fmap f xs

instance Foldable (Vec n) where 
  foldMap _ Nil        = mempty
  foldMap f (x ::: xs) = f x `mappend` foldMap f xs
  
instance Traversable (Vec n) where 
  traverse _  Nil       = pure Nil
  traverse f (x ::: xs) = (:::) <$> f x <*> traverse f xs

len :: Vec n a -> Nat n
len Nil        = Zro
len (_ ::: xs) = Suc (len xs)

get :: F.Nat n -> Vec n t -> t
get F.Zro     (x ::: _) = x
get (F.Suc n) (_ ::: r) = get n r
get _         _         = error "Impossible!"