module Environment.Plain where

import MyPrelude 

import Variable.Plain

type Env a = [a]

get :: Monad m => Var -> Env a -> m a
get Zro     (x : _ ) = return x
get (Suc n) (_ : xs) = get n xs
get _       []       = fail "Scope Error!" 