module Existential where

data Exs0 :: (* -> *) -> * where
  Exs0 :: t -> tf t -> Exs0 tf

data Exs1 :: (k -> *) -> (k -> *) -> * where
  Exs1 :: c1 t -> tf t -> Exs1 c1 tf

data ExsSin :: (k -> *) -> * where
  ExsSin :: st t -> ExsSin st

data Exs2 :: (k -> k1 -> *) -> (k -> *) -> (k1 -> *) -> * where
  Exs2 :: c2 t1 t2 -> tf1 t1 -> tf2 t2 -> Exs2 c2 tf1 tf2

mapSin :: (forall t. st t -> st' t') -> ExsSin st -> ExsSin st'
mapSin f (ExsSin t) = ExsSin (f t)