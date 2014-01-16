{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Unification where

-- A type class providing operations required for type unification. The first 
-- argument (m) provides the monad required for unification of types (t).       
class Monad m => Uni a m where 
  tCon   :: String -> [a] -> m a
  eql    :: a -> a -> m ()
  eqlCon :: String -> a -> m [a]