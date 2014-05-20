module Optimization 
       (OptOne(..),(<$>),(<*>),pure,(<$@>),(<*@>),Chg(..),chg,opt,optImp) 
       where

import Prelude ()
import MyPrelude

import qualified Variable.Plain  as VP
import qualified Variable.Scoped as VS
import qualified Variable.Typed  as VT
import ChangeMonad

class OptOne a r where
  optOne :: a -> r -> Chg a

instance OptOne Integer       r where
  optOne = const . pure

instance OptOne Bool          r where
  optOne = const . pure

instance OptOne Float         r where
  optOne = const . pure

instance OptOne VP.Var        r where
  optOne = const . pure

instance OptOne (VS.Var n)    r where
  optOne = const . pure

instance OptOne (VT.Var rr t) r where
  optOne = const . pure

opt :: OptOne a r => a -> r -> a
opt x r = tilNotChg (flip optOne r) x

optImp :: (?r :: r , OptOne a r) => a -> Chg a
optImp x = optOne x  ?r

infixl 4 <$@>
(<$@>) :: (?r :: r , OptOne a r) => (a -> b) -> a -> Chg b          
el <$@> er = el <$> optOne er ?r

infixl 4 <*@>
(<*@>) :: (?r :: r , OptOne a r) => Chg (a -> b) -> a -> Chg b
el <*@> er = el <*> optOne er ?r