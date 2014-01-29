{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, ImplicitParams #-}
module Conversion(Cnv(..),(<$>),(<$@>),(<*>),(<*@>),pure) where

import ErrorMonad
import Control.Applicative.Recursion

class Cnv a b where
  cnv :: a -> ErrM b
  
  