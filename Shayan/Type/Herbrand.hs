{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, PolyKinds, TypeOperators, DataKinds #-}
module Type.Herbrand where

import qualified Variable as G
import Data.Nat 
import Data.Vector
import Data.Foldable

data Typ r where 
   App :: G.Var r n -> Vec n (Typ r) -> Typ r
   Mta :: Nat -> Typ r  

-- Type equality constraint
infixr 6 :~:
data HerCon r = Typ r :~: Typ r 


-- Collecting metavariables in a typ
mtas :: Typ r -> [Nat]
mtas (Mta i)    = [i]
mtas (App _ ts) = foldMap mtas ts 

-- Subtitution of a metavariable ([i := t]) in a type 
appT :: Nat -> Typ r -> Typ r -> Typ r
appT i t (App c vs)          = App c (appT i t `fmap` vs)  
appT i t (Mta j) | j == i    = t
                 | otherwise = Mta j           

-- Subtitution of a metavariable ([i := t]) in a list of types 
appTs :: Nat -> Typ r -> [Typ r] -> [Typ r]
appTs i t = map (appT i t)

-- Applying a list of substitions for metavariables
appTtas :: [(Nat , Typ r)] -> Typ r -> Typ r
appTtas ttas t = Prelude.foldl (\ ta (i , t') -> appT i t' ta) t ttas 

intVar :: G.Var ('Zro ': r) 'Zro
intVar = G.Zro

arrVar :: G.Var (EnvIntArr r) ('Suc ('Suc 'Zro))
arrVar = G.Suc G.Zro  

bolVar :: G.Var (EnvIntArr ('Zro ': r)) 'Zro
bolVar = G.Suc (G.Suc G.Zro)

tplVar :: G.Var (EnvIntArr ('Zro ': 'Suc ('Suc 'Zro) ': r)) 
          ('Suc ('Suc 'Zro))
tplVar = G.Suc (G.Suc (G.Suc G.Zro))

aryVar :: G.Var (EnvFld r) ('Suc 'Zro)
aryVar = G.Suc (G.Suc (G.Suc (G.Suc G.Zro)))

int :: Typ (EnvIntArr r)
int = App intVar Nil

arr :: Typ (EnvIntArr r) -> Typ (EnvIntArr r) -> Typ (EnvIntArr r) 
arr ta tb = App arrVar (ta ::: (tb ::: Nil))

bol :: Typ (EnvIntArr ('Zro ': r))
bol = App bolVar Nil

tpl :: r' ~ (EnvIntArr ('Zro ': 'Suc ('Suc 'Zro) ': r)) => 
       Typ r' -> Typ r' -> Typ r'
tpl tf ts = App tplVar (tf ::: (ts ::: Nil))       

ary :: Typ (EnvFld r) -> Typ (EnvFld r)
ary ta = App aryVar (ta ::: Nil)  

type EnvIntArr r = 'Zro ': 'Suc ('Suc 'Zro) ': r 
type EnvFld    r = EnvIntArr ('Zro ': 'Suc ('Suc 'Zro) 
                              ': 'Suc 'Zro ': r)
