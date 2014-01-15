{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Conversion where

-- Convert from ADT to GADT representation.
-- Simply-typed lambda calculus with de Bruijn indicies
-- with integer constants and addition.
-- Philip Wadler and Shayan Najd, November 2013


import Prelude hiding (exp)
import qualified ADT  as U
import GADT hiding (run,test)
import qualified GADT as T

-- Proof of Equality
data Eql a b where
  Rfl :: Eql a a 
  --Reflexivity as a proof of equality

-- Equality of Types
eqlTyp :: Monad m => Typ t -> Typ t' -> m (Eql t t')
eqlTyp Int         Int           = return Rfl
eqlTyp (Arr t1 t2) (Arr t1' t2') = do Rfl <- eqlTyp t1 t1'
                                      Rfl <- eqlTyp t2 t2'
                                      return Rfl                    
eqlTyp _            _             = fail "Type Error!"

-- Equality of Environments
eqlEnv :: Monad m => Env e -> Env e' -> m (Eql e e')
eqlEnv Emp       Emp         = return Rfl
eqlEnv (Ext e t) (Ext e' t') = do Rfl <- eqlEnv e e'
                                  Rfl <- eqlTyp t t'
                                  return Rfl
eqlEnv _ _                   = fail "Scope Error!"     

-- Typ Wrapper (exitentials)
data WTyp where
  WTyp :: Typ a -> WTyp

-- Environment Wrapper (exitentials)
data WEnv where
  WEnv :: Env a -> WEnv

-- Variable Wrapper (exitentials)
data WVar where
  WVar :: Var e a -> Env e -> Typ a -> WVar

-- Expression Wrapper (exitentials)
data WExp where
  WExp :: Exp e a -> Env e -> Typ a -> WExp

-- Type Translation from ADTs to GADTs
typ :: Monad m => U.Typ -> m WTyp
typ U.Int          = return (WTyp Int)
typ (U.Arr ta tr)  = do 
  WTyp ta' <- typ ta
  WTyp tr' <- typ tr
  return (WTyp (Arr ta' tr'))

-- Environment Translation from ADTs to GADTs
env :: Monad m => [U.Typ] -> m WEnv
env []      = return (WEnv Emp)
env (t : r) = do 
  WTyp t' <- typ t
  WEnv r' <- env r
  return (WEnv (Ext r' t'))

-- Variable Translation from ADTs to GADTs
var :: Monad m => U.Var -> [U.Typ] -> m WVar
var U.Zro     (t : r) = do 
  WEnv r' <- env r
  WTyp t' <- typ t
  return (WVar Zro (Ext r' t') t')
var (U.Suc x) (t : r) = do 
  WVar x' r' tr <- var x r
  WTyp t'       <- typ t
  return (WVar (Suc x') (Ext r' t') tr)
var _          []     = fail "Impossible!"  

-- Expression Translation from ADTs to GADTs
exp :: Monad m => U.Exp -> [U.Typ] -> m WExp
exp (U.Con i)     r = do WEnv r' <- env r
                         return (WExp (Con i) r' Int)
exp (U.Var x)     r = do WVar x' r' t' <- var x r
                         return (WExp (Var x') r' t')
exp (U.Abs ta eb) r = do WTyp ta' <- typ ta
                         WExp eb' (Ext r' ta'') tr <- exp eb (ta : r)
                         Rfl <- eqlTyp ta' ta''
                         return (WExp (Abs ta' eb') r' (Arr ta' tr))
exp (U.App ef ea) r = do WExp ef' r' (Arr t' u') <- exp ef r
                         WExp ea' r'' t'' <- exp ea r
                         Rfl <- eqlEnv r' r''
                         Rfl <- eqlTyp t' t''
                         return (WExp (App ef' ea') r' u')
exp (U.Add el er) r = do WExp el' r'  Int <- exp el r
                         WExp er' r'' Int <- exp er r
                         Rfl <- eqlEnv r' r''
                         return (WExp (Add el' er') r' Int)

run :: U.Exp -> Int
run m  = case (do WExp m' Emp Int <- exp m []
                  return (T.run m' ())) of
           Right i -> i
           Left  s -> error s   
 
test :: Bool
test = (run U.four == 4)

main :: Bool
main = U.test && T.test && test
