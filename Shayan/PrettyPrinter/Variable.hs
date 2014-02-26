{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module PrettyPrinter.Variable where

import Variable.GADT
import Conversion.Nat ()
import Conversion
import ErrorMonad
import Data.Nat 

instance Show (Var e t) where
 show =  ("x" ++) . show . (frmRgt . cnv :: Var e t -> Nat)

