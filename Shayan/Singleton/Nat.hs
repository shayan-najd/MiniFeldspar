{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleContexts, TypeFamilies
           , DataKinds, TypeOperators #-}
module Singleton.Nat where
 
import Prelude (Maybe,fail,return)
import Singleton
import qualified Data.Nat as N
  
data Nat n where
  Zro :: Nat N.Zro
  Suc :: Nat n -> Nat (N.Suc n)  
  
type instance Trm (Nat N.Zro)     = Maybe ()  
type instance Trm (Nat (N.Suc n)) = Maybe (Trm (Nat n))
  
instance HasSin Nat N.Zro where  
  sin = Zro
  
instance (HasSin Nat n) => HasSin Nat (N.Suc n) where  
  sin = Suc sin
  
instance EqlSin Nat where
  eqlSin Zro     Zro      = return Rfl
  eqlSin (Suc n) (Suc n') = do Rfl <- eqlSin n n' 
                               return Rfl 
  eqlSin _       _        = fail "Type Error!"                           
  
  
  