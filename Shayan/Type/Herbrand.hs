{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Type.Herbrand where

import qualified Variable.GADT as G
import qualified Data.Nat as N
import Data.Vector
import Data.Foldable
import Data.Nat.GADT  

data Typ r where 
   App :: G.Var r n -> Vec n (Typ r) -> Typ r
   Mta :: N.Nat -> Typ r  

-- Collecting metavariables in a typ
mtas :: Typ r -> [N.Nat]
mtas (Mta i)    = [i]
mtas (App _ ts) = foldMap mtas ts 

-- Subtitution of a metavariable ([i := t]) in a type 
appT :: N.Nat -> Typ r -> Typ r -> Typ r
appT i t (App c vs)          = App c (appT i t `fmap` vs)  
appT i t (Mta j) | j == i    = t
                 | otherwise = Mta j           

-- Subtitution of a metavariable ([i := t]) in a list of types 
appTs :: N.Nat -> Typ r -> [Typ r] -> [Typ r]
appTs i t = map (appT i t)

-- Applying a list of substitions for metavariables
appTtas :: [(N.Nat , Typ r)] -> Typ r -> Typ r
appTtas ttas t = Prelude.foldl (\ ta (i , t') -> appT i t' ta) t ttas 

int :: G.Var (Zro,r) Zro
int = G.Zro

arr :: G.Var (EnvIntArr r) (Suc (Suc Zro))
arr = G.Suc G.Zro  

bol :: G.Var (EnvIntArr (Zro , r)) Zro
bol = G.Suc (G.Suc G.Zro)

tpl :: G.Var (EnvIntArr (Zro , (Suc (Suc Zro) , r))) (Suc (Suc Zro))
tpl = G.Suc (G.Suc (G.Suc G.Zro))

ary :: G.Var (EnvFld r) (Suc Zro)
ary = G.Suc (G.Suc (G.Suc (G.Suc G.Zro)))

type EnvIntArr r = (Zro               -- Int has no  argument
                  ,(Suc (Suc Zro)     -- Arr has two arguments
                  ,r)) 
                   
type EnvFld    r = EnvIntArr 
                   (Zro
                  ,(Suc (Suc Zro) 
                  ,(Suc Zro 
                  ,r)))
