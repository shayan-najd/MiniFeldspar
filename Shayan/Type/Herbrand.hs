{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Type.Herbrand where

import qualified Variable.GADT as G
import qualified Data.Nat.GADT as S
import Data.Nat
import Data.Vector
import Data.Foldable

data Typ r where 
   App :: S.Nat n -> G.Var r n -> Vec n (Typ r) -> Typ r
   Mta :: Nat -> Typ r  

-- Collecting metavariables in a typ
mtas :: Typ r -> [Nat]
mtas (Mta i)      = [i]
mtas (App _ _ ts) = foldMap mtas ts 

-- Subtitution of a metavariable ([i := t]) in a type 
appT :: Nat -> Typ r -> Typ r -> Typ r
appT i t (App n c vs)        = App n c (appT i t `fmap` vs)  
appT i t (Mta j) | j == i    = t
                 | otherwise = Mta j           

-- Subtitution of a metavariable ([i := t]) in a list of types 
appTs :: Nat -> Typ r -> [Typ r] -> [Typ r]
appTs i t = map (appT i t)

-- Applying a list of substitions for metavariables
appTtas :: [(Nat , Typ r)] -> Typ r -> Typ r
appTtas ttas t = Prelude.foldl (\ ta (i , t') -> appT i t' ta) t ttas 