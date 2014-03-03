module Control.Applicative.Recursion ((<$>),(<$@>),(<*>),(<*@>),pure,join) where

import Control.Applicative (Applicative,(<$>),(<*>),pure)
import Control.Monad (join)

infixl 4 <$@>
(<$@>) :: (Applicative m , ?cnv :: a -> m a') => (a' -> b) -> a -> m b          
el <$@> er = el <$> ?cnv er

infixl 4 <*@>
(<*@>) :: (Applicative m , ?cnv :: a -> m a') => m (a' -> b) -> a -> m b
el <*@> er = el <*> ?cnv er