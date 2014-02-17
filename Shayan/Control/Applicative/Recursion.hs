{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ImplicitParams #-}
module Control.Applicative.Recursion ((<$>),(<$@>),(<*>),(<*@>),pure) where

import Control.Applicative (Applicative,(<$>),(<*>),pure)

infixl 4 <$@>
(<$@>) :: (Applicative m , ?cnv :: a -> m a') => (a' -> b) -> a -> m b          
el <$@> er = el <$> ?cnv er

infixl 4 <*@>
(<*@>) :: (Applicative m , ?cnv :: a -> m a') => m (a' -> b) -> a -> m b
el <*@> er = el <*> ?cnv er