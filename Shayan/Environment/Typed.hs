module Environment.Typed(Env(Emp,Ext),fmap,foldMap,traverse,len,get) where

import MyPrelude hiding (mapM,fmap,traverse,foldMap)

import Variable.Typed

import Singleton
import qualified Nat.GADT as NG

data Env :: (k -> *) -> [k] -> * where
  Emp :: Env tf '[]
  Ext :: tf t -> Env tf e -> Env tf (t ': e)

fmap :: (forall t. tfa t -> tfb t) -> Env tfa r -> Env tfb r
fmap _ Emp        = Emp
fmap f (Ext x xs) = Ext (f x) (fmap f xs)   

foldMap :: Monoid m  => 
           (forall t. tfa t -> m) -> Env tfa r -> m
foldMap  _ Emp        = mempty
foldMap  f (Ext x xs) = mappend (f x) (foldMap f xs)

traverse :: Applicative m => 
            (forall t. tfa t -> m (tfb t)) -> Env tfa r -> m (Env tfb r)
traverse _ Emp        = pure Emp
traverse f (Ext x xs) = Ext <$> f x <*> traverse f xs

len :: Env ef r -> NG.Nat (Len r)
len Emp        = NG.Zro
len (Ext _ xs) = NG.Suc (len xs)
                  
get :: Var r t -> Env tf r -> tf t
get Zro     (Ext x  _ ) = x
get (Suc n) (Ext _  xs) = get n xs
get _       Emp         = impossible

-- add :: (?env :: Env tf r') => Var r t -> Var (Add r' r) t
-- add v = case ?env of
--   Emp      -> v
--   Ext _ xs -> let ?env = xs in Suc (add v) 
 
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