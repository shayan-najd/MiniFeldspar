{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, ImplicitParams #-}
module Conversion(Cnv(..),(<$>),(<$@>),(<*>),(<*@>),pure) where

import ErrorMonad
import Control.Applicative (Applicative,(<$>),(<*>),pure)

class Cnv a b where
  cnv :: a -> ErrM b
  
infixl 4 <$@>
(<$@>) :: (Applicative m , ?cnv :: a -> m a') => (a' -> b) -> a -> m b          
el <$@> er = el <$> ?cnv er

infixl 4 <*@>
(<*@>) :: (Applicative m , ?cnv :: a -> m a') => m (a' -> b) -> a -> m b
el <*@> er = el <*> ?cnv er
  
  