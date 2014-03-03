module PrettyPrinter.Nat where
import Data.Nat
import Conversion.Nat ()
import ErrorMonad
import Conversion

instance Show Nat where
 show = show . frmRgt . (cnv :: Nat -> ErrM Int)