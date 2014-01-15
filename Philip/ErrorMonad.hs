{-# OPTIONS_GHC -Wall #-}
module ErrorMonad where

import Control.Applicative (Applicative(..))

-- Using this monad has the following benefits:
--   (a) it will not have the problem of error producing code being ignored
--       due to Haskell's laziness. For example, in the following the error 
--       producing code is ignored:
--       $> take 1 [0,error "Disaster!"] 
--          [0]
--   (b) the type forces the programmer to write a handler for the potential 
--       error    
--   (c) it keeps the error message
data ErrM t = Rgt t 
            | Lft String
              deriving (Eq , Show)

instance Functor ErrM where
  fmap f (Rgt x) = Rgt (f x)
  fmap _ (Lft x) = Lft x
  
instance Applicative ErrM where
  pure      = return
  e1 <*> e2 = do v1 <- e1  
                 v2 <- e2
                 return (v1 v2)

instance Monad ErrM where
  return      = Rgt
  Lft l >>= _ = Lft l
  Rgt r >>= k = k r
  fail x      = Lft x
