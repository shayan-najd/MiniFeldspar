module Environment.Plain (Env,pattern Emp,pattern Ext,len,get) where

import MyPrelude

import Variable.Plain

import qualified Nat.ADT  as NA

type Env a = [a]

pattern Emp      = []
pattern Ext x xs = x : xs

len :: Env a -> NA.Nat
len Emp        = NA.Zro
len (Ext _ xs) = NA.Suc (len xs)
len _          = impossible

get :: Monad m => Var -> Env a -> m a
get Zro     (x : _ ) = return x
get (Suc n) (_ : xs) = get n xs
get _       []       = fail "Scope Error!"