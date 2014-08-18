module Environment.Map (Env,pattern Emp,pattern Ext,len,get) where
 
import MyPrelude 

import qualified Nat.ADT  as NA

type Env a b = [(a , b)]

pattern Emp      = []
pattern Ext x xs = x : xs

len :: Env a b -> NA.Nat
len Emp        = NA.Zro
len (Ext _ xs) = NA.Suc (len xs)
len _          = impossible

get :: (Monad m , Eq a) => a -> Env a b -> m b
get = (maybe (fail "Scope Error!") return . ) . lookup