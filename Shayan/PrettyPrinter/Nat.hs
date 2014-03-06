module PrettyPrinter.Nat where
import qualified Data.Nat as N
import qualified Data.Fin as F

import Conversion.Nat ()
import ErrorMonad
import Conversion

instance Show N.Nat where
 show = show . frmRgt . (cnv :: N.Nat -> ErrM Int)
 
instance Show (F.Nat n) where
 show = show . frmRgt . (cnv :: F.Nat n -> ErrM Int)
 