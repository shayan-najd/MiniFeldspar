{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Conversion where

import Prelude hiding (exp)
import qualified ADT  as U
import qualified GADT as T

-- Proof of Equality
data Eql a b where
  Rfl :: Eql a a 
  --Reflexivity as a proof of equality

-- Equality of Types
eqlTyp :: T.Typ t -> T.Typ t' -> U.ErrM (Eql t t')
eqlTyp T.Int         T.Int           = return Rfl
eqlTyp (T.Arr t1 t2) (T.Arr t1' t2') = do Rfl <- eqlTyp t1 t1'
                                          Rfl <- eqlTyp t2 t2'
                                          return Rfl                    
eqlTyp _             _               = fail "Type Error!"

-- Equality of Environments
eqlEnv :: T.Env e -> T.Env e' -> U.ErrM (Eql e e')
eqlEnv T.Emp         T.Emp           = return Rfl
eqlEnv (t `T.Ext` e) (t' `T.Ext` e') = do Rfl <- eqlEnv e e'
                                          Rfl <- eqlTyp t t'
                                          return Rfl
eqlEnv _             _               = fail "Scope Error!"     

-- Typ Wrapper (existentials)
data Typ where
  Typ :: T.Typ t -> Typ

-- Environment Wrapper (existentials)
data Env where
  Env :: T.Env e -> Env

-- Variable Wrapper (existentials)
data Var where
  Var :: T.Var e t -> T.Env e -> T.Typ t -> Var

-- Expression Wrapper (existentials)
data Exp where
  Exp :: T.Exp e t -> T.Env e -> T.Typ t -> Exp

-- Type Translation from ADTs to GADTs
typ :: U.Typ -> U.ErrM Typ
typ U.Int          = return (Typ T.Int)
typ (U.Arr ta tr)  = do Typ ta' <- typ ta
                        Typ tr' <- typ tr
                        return (Typ (T.Arr ta' tr'))

-- Environment Translation from ADTs to GADTs
env :: [U.Typ] -> U.ErrM Env
env []      = return (Env T.Emp)
env (t : r) = do Typ t' <- typ t
                 Env r' <- env r
                 return (Env (t' `T.Ext` r'))

-- Variable Translation from ADTs to GADTs
var :: U.Var -> [U.Typ] -> U.ErrM Var
var U.Zro     (t : r) = do Env r' <- env r
                           Typ t' <- typ t
                           return (Var T.Zro (t' `T.Ext` r') t')
var (U.Suc x) (t : r) = do Var x' r' tr <- var x r
                           Typ t'       <- typ t
                           return (Var (T.Suc x') (t' `T.Ext` r') tr)
var _          []     = fail "Impossible!"  
                        -- the redundant pattern checker cannot guess that
                        -- and instance of Var never lets the environment to
                        -- to be empty.

-- Expression Translation from ADTs to GADTs
exp :: U.Exp -> [U.Typ] -> U.ErrM Exp
exp (U.Con i)     r = do Env r' <- env r
                         return (Exp (T.Con i) r' T.Int)
exp (U.Var x)     r = do Var x' r' t' <- var x r
                         return (Exp (T.Var x') r' t')
exp (U.Abs ta eb) r = do Typ ta' <- typ ta
                         Exp eb' (ta'' `T.Ext` r') tb <- exp eb (ta : r)
                         Rfl <- eqlTyp ta' ta''
                         return (Exp (T.Abs ta' eb') r' (T.Arr ta' tb))
exp (U.App ef ea) r = do Exp ef' rf (T.Arr ta tb) <- exp ef r
                         Exp ea' ra ta'           <- exp ea r
                         Rfl <- eqlEnv rf ra
                         Rfl <- eqlTyp ta ta'
                         return (Exp (T.App ef' ea') rf tb)
exp (U.Add el er) r = do Exp el' rl T.Int <- exp el r
                         Exp er' rr T.Int <- exp er r
                         Rfl <- eqlEnv rl rr
                         return (Exp (T.Add el' er') rl T.Int)

evl :: U.Exp -> Int
evl m  = case (do Exp m' T.Emp T.Int <- exp m []
                  return (T.evl m' ())) of
           Right i -> i
           Left  s -> error s   
 
test :: Bool
test = evl U.four == 4

main :: Bool
main = U.test && T.test && test
