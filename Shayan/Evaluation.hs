{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation where

import ErrorMonad

type family Val e
type family Env e

class Evl e where
  evl :: e -> Env e -> ErrM (Val e)