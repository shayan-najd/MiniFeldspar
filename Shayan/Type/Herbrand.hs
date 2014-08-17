module Type.Herbrand where

import MyPrelude

import Variable.Typed
import Environment.Scoped 
import Nat.TH

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

type EnvFld r = $(natT 0 "NA.") ': $(natT 2 "NA.") ': $(natT 0 "NA.") ': 
                $(natT 2 "NA.") ': $(natT 1 "NA.") ': $(natT 0 "NA.") ': 
                $(natT 0 "NA.") ': r
  
pattern Int       = App $(natP 0 "") Emp
pattern Arr ta tb = App $(natP 1 "") (Ext ta (Ext tb Emp))
pattern Bol       = App $(natP 2 "") Emp
pattern Tpl tf ts = App $(natP 3 "") (Ext tf (Ext ts Emp))       
pattern Ary ta    = App $(natP 4 "") (Ext ta  Emp)  
pattern Flt       = App $(natP 5 "") Emp
pattern Cmx       = App $(natP 6 "") Emp