{-# LANGUAGE MultiParamTypeClasses #-}
module Conversion where

import ErrorMonad

class Cnv a b where
  cnv :: a -> ErrM b
