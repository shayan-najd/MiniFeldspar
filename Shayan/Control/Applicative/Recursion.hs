module Control.Applicative.Recursion where

import Prelude ()
import MyPrelude 
 
infixl 4 <$@>
(<$@>) :: (Applicative m , ?cnv :: a -> m a') => (a' -> b) -> a -> m b          
el <$@> er = el <$> ?cnv er

infixl 4 <*@>
(<*@>) :: (Applicative m , ?cnv :: a -> m a') => m (a' -> b) -> a -> m b
el <*@> er = el <*> ?cnv er