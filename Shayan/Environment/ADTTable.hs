module Environment.ADTTable where
 
type Env a b = [(a , b)]

-- Extraction of values from environment
get :: (Monad m , Eq a ) => a -> Env a b -> m b
get = (maybe (fail "Scope Error!") return . ) . lookup