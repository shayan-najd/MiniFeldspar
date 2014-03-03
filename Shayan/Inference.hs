{-# LANGUAGE Rank2Types #-}
module Inference where

import qualified TypeChecking as Chk
import ErrorMonad
import Data.Nat
import Type.Herbrand
import Solver (slv)
import Data.Traversable (Traversable,traverse)
import Control.Monad.State (runState,put,get,execState,evalState)
import Conversion
import Environment.ADT hiding (get)
import Data.Foldable(toList)
import Conversion.Nat ()

mxm :: [Nat] -> Nat
mxm [] = Zro
mxm l  = maximum l

ind :: Traversable ef => 
       ef () -> ef Nat
ind = flip evalState Zro .  
      traverse (const (do i <- get
                          put (Suc i)
                          return i))

typInf :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) => 
       ef () -> [Typ r] -> ErrM (ef (Typ r))       
typInf e r = let en = ind e
                 et = fmap Mta en
             in inf et r
  
inf :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) => 
       ef (Typ r) -> [Typ r] -> ErrM (ef (Typ r))
inf e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (i' , cs) = execState (Chk.chk e r) (mts , [])
             mTs <- slv (map Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs -- substituitions
             return (appTtas ttas `fmap` e)
  
chk :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) => 
       ef (Typ r) -> [Typ r] -> ErrM (Typ r)
chk e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (t , (i' , cs)) = runState (Chk.chk e r) (mts , [])
             mTs <- slv (map Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs -- substituitions
             if length [() | Mta _ <- mTs] == 0
               then return (appTtas ttas t)
               else fail "Type Error!"   


typChk :: forall ef t. 
          (Chk.Chk ef , Traversable ef, Cnv t (Typ (Chk.Cns ef))
          , Cnv (Typ (Chk.Cns ef)) t)  => 
          ef t -> Env t-> ErrM t
typChk e r = do r' :: Env (Typ (Chk.Cns ef)) <- traverse cnv r          
                e' :: ef  (Typ (Chk.Cns ef)) <- traverse cnv e
                t <- chk e' r'
                cnv t