module Evaluation (Val,Env,Evl(..),ErrM,frmRgt,module C) where

import ErrorMonad
import Control.Applicative.Recursion as C

type family Val e
type family Env e

class Evl e where
  evl :: e -> Env e -> ErrM (Val e)