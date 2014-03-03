module ChangeMonad where

import Control.Applicative

-- change monad
data Chg a = Chg Bool a
             deriving Functor

instance Applicative Chg where
  pure      = return
  af <*> aa = do f <- af 
                 a <- aa
                 pure (f a)

instance Monad Chg where
  return           = Chg False 
  (Chg b x) >>= f = let Chg b' x' = f x 
                     in Chg (b || b') x'    
chg :: a -> Chg a
chg = Chg True

tilNotChg :: (a -> Chg a) -> a -> a 
tilNotChg f x = case f x of
  Chg False _  -> x
  Chg True  x' -> tilNotChg f x'
