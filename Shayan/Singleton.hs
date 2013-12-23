{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Singleton where

import ErrorMonad

data Prx t = Prx

class Sig s t where
  sig :: Prx t -> ErrM (s t)