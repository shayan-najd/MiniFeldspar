{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Singleton where

import ErrorMonad
  
class Sin s t where
  sin :: ErrM (s t)
  
  