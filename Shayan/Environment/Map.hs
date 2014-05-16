module Environment.Map where
 
import Prelude ()
import MyPrelude 

type Env a b = [(a , b)]

get :: (Monad m , Eq a) => a -> Env a b -> m b
get = (maybe (fail "Scope Error!") return . ) . lookup