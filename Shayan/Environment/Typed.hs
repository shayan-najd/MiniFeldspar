module Environment.Typed where

import Prelude ()
import MyPrelude hiding (mapM)

import Singleton

import Variable.Typed

import qualified Nat.GADT as NG

data Env :: (k -> *) -> [k] -> * where
  Emp :: Env tf '[]
  Ext :: tf t -> Env tf e -> Env tf (t ': e)

len :: Env ef r -> NG.Nat (Len r)
len Emp        = NG.Zro
len (Ext _ xs) = NG.Suc (len xs)
 
add :: (?env :: Env tf r') => Var r t -> Var (Add r' r) t
add v = case ?env of
  Emp      -> v
  Ext _ xs -> let ?env = xs in Suc (add v) 
                  
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