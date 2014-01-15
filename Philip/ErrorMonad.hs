{-# OPTIONS_GHC -Wall #-}
module ErrorMonad where

import Control.Applicative (Applicative(..))

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
