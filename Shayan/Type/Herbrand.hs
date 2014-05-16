module Type.Herbrand where

import Prelude ()
import MyPrelude

import Variable.Typed
import Environment.Scoped 

import qualified Nat.ADT as NA

data Typ :: [NA.Nat] -> * where 
   App :: Var r n -> Env n (Typ r) -> Typ r
   Mta :: NA.Nat -> Typ r  

deriving instance Show (Typ r)

infixr 6 :~:
data HerCon r = Typ r :~: Typ r 

deriving instance Show (HerCon r)

appC :: NA.Nat -> Typ r -> HerCon r -> HerCon r
appC i t (t1 :~: t2) = appT i t t1 :~: appT i t t2

appCs :: NA.Nat -> Typ r -> [HerCon r] -> [HerCon r]
appCs i t = fmap (appC i t)

mtas :: Typ r -> [NA.Nat]
mtas (Mta i)    = [i]
mtas (App _ ts) = foldMap mtas ts 

appT :: NA.Nat -> Typ r -> Typ r -> Typ r
appT i t (App c vs)          = App c (fmap (appT i t) vs)  
appT i t (Mta j) | j == i    = t
                 | otherwise = Mta j           

appTs :: NA.Nat -> Typ r -> [Typ r] -> [Typ r]
appTs i t = fmap (appT i t)

appTtas :: [(NA.Nat , Typ r)] -> Typ r -> Typ r
appTtas ttas t = foldl (\ ta (i , t') -> appT i t' ta) t ttas 

type EnvFld r = NA.N0 ': NA.N2 ': NA.N0 ': NA.N2 ': 
                NA.N1 ': NA.N0 ': NA.N0 ': r
{-                
intVar :: Var (EnvFld r) NA.Zro
intVar = Zro                   

arrVar :: Var (EnvFld r) (NA.Suc (NA.Suc NA.Zro))
arrVar = Suc Zro  

bolVar :: Var (EnvFld r) NA.Zro
bolVar = Suc (Suc Zro)

tplVar :: Var (EnvFld r) (NA.Suc (NA.Suc NA.Zro))
tplVar = Suc (Suc (Suc Zro))

aryVar :: Var (EnvFld r) (NA.Suc NA.Zro)
aryVar = Suc (Suc (Suc (Suc Zro)))

fltVar :: Var (EnvFld r) NA.Zro
fltVar = Suc (Suc (Suc (Suc (Suc Zro))))
-}                       
 
pattern Int       = App Zro Emp
pattern Arr ta tb = App (Suc Zro) (Ext ta (Ext tb Emp))
pattern Bol       = App (Suc (Suc Zro)) Emp
pattern Tpl tf ts = App (Suc (Suc (Suc Zro))) (Ext tf (Ext ts Emp))       
pattern Ary ta    = App (Suc (Suc (Suc (Suc Zro)))) (Ext ta  Emp)  
pattern Flt       = App (Suc (Suc (Suc (Suc (Suc Zro))))) Emp
pattern Cmx       = App (Suc (Suc (Suc (Suc (Suc (Suc Zro)))))) Emp

