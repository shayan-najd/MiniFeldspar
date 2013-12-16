{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs
           , ScopedTypeVariables, FlexibleContexts #-}

import qualified Expression.Existential as W

import qualified Type.ADTSimple as AS
import qualified Type.GADT as G

import Conversion
import Expression.Conversion ()
import Type.Conversion ()
import Variable.Conversion ()
import Environment.Conversion ()

import qualified Environment.ADT as A
import qualified Environment.GADT as G

import qualified ADTUntyped as U
import qualified ADTChurchMonomorphic as ACM
import qualified ADTChurchPolymorphic as ACP
import qualified ADTExplicitPolymorphic as E
import qualified GADT as G

import ErrorMonad

tstEvl :: Cnv a W.Exp => a -> Int
tstEvl e  = case (do W.Exp e' G.Emp G.Int <- cnv e
                     return (G.evl e' ())) of
              Rgt i -> i
              Lft s -> error s   

tstUFour :: Bool
tstUFour = curry tstEvl U.four ([] :: A.Env AS.Typ) == 4

tstACMFour :: Bool
tstACMFour = curry tstEvl ACM.four ([] :: A.Env AS.Typ) == 4

tstACPFour :: Bool
tstACPFour = curry tstEvl ACP.four ([] :: A.Env AS.Typ) == 4

tstEFour :: Bool
tstEFour = curry tstEvl E.four ([] :: A.Env AS.Typ) == 4

main :: IO ()
main = if (tstUFour && tstACMFour && tstACPFour && tstEFour &&
           U.test   && ACM.test   && ACP.test   && E.test)
       then print "Pass!"
       else print "Fail!"