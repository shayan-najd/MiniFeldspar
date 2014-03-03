module PrettyPrinter.Variable where

import Variable
import Conversion.Nat ()
import Conversion
import Data.Nat 
import PrettyPrinter.Nat ()

instance Show (Var e t) where
 show =  ("x" ++) . show . (frmRgt . cnv :: Var e t -> Nat)

