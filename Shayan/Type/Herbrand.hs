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

intVar :: Var (NA.Zro ': r) NA.Zro
intVar = Zro

arrVar :: Var (EnvIntArr r) (NA.Suc (NA.Suc NA.Zro))
arrVar = Suc Zro  

bolVar :: Var (EnvIntArr (NA.Zro ': r)) NA.Zro
bolVar = Suc (Suc Zro)

tplVar :: Var (EnvIntArr (NA.Zro ': NA.Suc (NA.Suc NA.Zro) ': r)) 
          (NA.Suc (NA.Suc NA.Zro))
tplVar = Suc (Suc (Suc Zro))

aryVar :: Var (EnvFld r) (NA.Suc NA.Zro)
aryVar = Suc (Suc (Suc (Suc Zro)))

int :: Typ (EnvIntArr r)
int = App intVar Emp

arr :: Typ (EnvIntArr r) -> Typ (EnvIntArr r) -> Typ (EnvIntArr r) 
arr ta tb = App arrVar (Ext ta (Ext tb Emp))

bol :: Typ (EnvIntArr (NA.Zro ': r))
bol = App bolVar Emp

tpl :: r' ~ (EnvIntArr (NA.Zro ': NA.Suc (NA.Suc NA.Zro) ': r)) => 
       Typ r' -> Typ r' -> Typ r'
tpl tf ts = App tplVar (Ext tf (Ext ts Emp))       

ary :: Typ (EnvFld r) -> Typ (EnvFld r)
ary ta = App aryVar (Ext ta  Emp)  

type EnvIntArr r = NA.Zro ': NA.Suc (NA.Suc NA.Zro) ': r 
type EnvFld    r = EnvIntArr (NA.Zro ': NA.Suc (NA.Suc NA.Zro) 
                              ': NA.Suc NA.Zro ': r)
