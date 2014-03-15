module ErrorMonad where

import Control.Applicative (Applicative(..))
import Data.Foldable
import Data.Traversable

data ErrM t = Rgt t 
            | Lft String
              
deriving instance Eq t   => Eq   (ErrM t)
deriving instance Show t => Show (ErrM t)
deriving instance Functor     ErrM
deriving instance Foldable    ErrM
deriving instance Traversable ErrM

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

frmRgt :: ErrM a -> a
frmRgt (Rgt x) = x
frmRgt (Lft s) = error s 