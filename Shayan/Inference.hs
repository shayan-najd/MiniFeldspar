{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs , FlexibleContexts #-}
module Inference where

import qualified TypeChecking as Chk
import ErrorMonad
import qualified Unification as U
import Unification (Uni)
import Conversion 
import InferenceMonad
import Variable.ADT
import Type.Herbrand
import Solver

import Prelude hiding (mapM)
import Data.Traversable
import Control.Monad.State (State,runState)

inf :: ( Chk.Chk e
       , e         ~ ef t
       , Traversable ef
       , Chk.Typ e ~ t
       , U.Mnd t   ~ (State (Nat , [HerCon r]))  
       , Uni t        
       , Cnv t       (Typ r)
       , Cnv (Typ r) t) =>
       (eu -> InfM r e) -> (eu , Chk.Env e) -> ErrM e
inf frmUExp (eu , r) = do 
  let (e' , (i , cs)) = runState 
                        (do e <- frmUExp eu -- conversion to an expression type
                                            -- with explicit fresh skolem vars 
                            _ <- Chk.chk e r
                            return e) 
                        (Zro , [])  
  mTs <- slv (map Mta [Zro .. pred i]) cs
  let ttas = zip [Zro ..] mTs -- substituitions
  e'' <- mapM cnv e'
  mapM cnv (appTtas ttas `fmap` e'')  

