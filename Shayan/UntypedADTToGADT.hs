{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module UntypedADTToGADT where

import qualified UntypedADT as U
import qualified GADT as T
import qualified UntypedADTToExplicitADT as UE
import qualified ExplicitADTToGADT as ET
import ADTToGADT  (Exp(..))
import Prelude hiding (exp)
import ErrorMonad (ErrM(..))
import Control.Monad((<=<))

-- Expression Translation from Untyped ADTs to GADTs
exp :: U.Exp -> ErrM Exp
exp = flip ET.exp [] <=< UE.inf 

evl :: U.Exp -> Int
evl e  = case (do Exp e' T.Emp T.Int <- exp e
                  return (T.evl e' ())) of
           Rgt i -> i
           Lft s -> error s   

test :: Bool
test = evl U.four == 4

main :: Bool
main = ET.main && test
