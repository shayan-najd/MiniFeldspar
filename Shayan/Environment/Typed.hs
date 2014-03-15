module Environment.Typed where

import Prelude ()
import MyPrelude hiding (mapM)

import Singleton

import Variable.Typed

import qualified Nat.ADT  as N
import qualified Nat.GADT as V

data Env :: (k -> *) -> [k] -> * where
  Emp :: Env tf '[]
  Ext :: tf t -> Env tf e -> Env tf (t ': e)

len :: Env ef r -> V.Nat (Len r)
len Emp        = V.Zro
len (Ext _ xs) = V.Suc (len xs)
 
get :: Var r t -> Env tf r -> tf t
get Zro     (Ext x  _ ) = x
get (Suc n) (Ext _  xs) = get n xs
get _       Emp         = impossible
 
cnvGEnvtoGVar ::  Env tf r -> Env (Var r) r
cnvGEnvtoGVar Emp        = Emp
cnvGEnvtoGVar (Ext _ xs) = Ext Zro (map Suc (cnvGEnvtoGVar xs))
 
map :: (forall t. tfa t -> tfb t) -> Env tfa r -> Env tfb r
map _ Emp        = Emp
map f (Ext x xs) = Ext (f x) (map f xs)   
  
mapM :: Applicative m => 
        (forall t. tfa t -> m (tfb t)) -> Env tfa r -> m (Env tfb r)
mapM _ Emp        = pure Emp
mapM f (Ext x xs) = Ext <$> f x <*> mapM f xs

type instance Trm '[]        = ()
type instance Trm (tf ': ts) = (Trm tf , Trm ts)
                   
type instance RevTrm ()        = '[]                   
type instance RevTrm (tf , ts) = (RevTrm tf ': RevTrm ts)                    
                                                     
instance HasSin (Env tf) '[] where
  sin = Emp
  
instance (HasSin tf t , HasSin (Env tf) ts) => HasSin (Env tf) (t ': ts) where
  sin = Ext sin sin
  
instance EqlSin tf => EqlSin (Env tf) where 
  eqlSin Emp       Emp         = return Rfl
  eqlSin (Ext t e) (Ext t' e') = do Rfl <- eqlSin e e'
                                    Rfl <- eqlSin t t'
                                    return Rfl
  eqlSin  _           _        = fail "Scope Error!"     
  
type family Len (l :: [k]) :: N.Nat where
  Len (x ': xs) = N.Suc (Len xs)
  Len '[]       = N.Zro 