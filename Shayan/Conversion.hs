module Conversion(Cnv(..),(<$>),(<$@>),(<*>),(<*@>),pure,ErrM,frmRgt) where

import ErrorMonad
import Control.Applicative.Recursion
 
class Cnv a b where
  cnv :: a -> ErrM b