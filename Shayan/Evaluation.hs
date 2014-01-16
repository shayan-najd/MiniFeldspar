{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation where

import ErrorMonad

class Evl e where
  type Val e
  type Env e
  evl :: e -> Env e -> ErrM (Val e)