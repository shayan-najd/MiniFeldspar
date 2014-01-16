{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Value.STLC.GADT where

import ErrorMonad

con :: Integer -> ErrM Integer
con = return


abs :: (a -> ErrM b) -> ErrM (a -> b)
abs f = return (\ va -> case f va of 
                         Rgt vb -> vb
                         Lft s  -> error s)

-- Application of two values
app :: (a -> b) -> a -> ErrM b
app = (return .) . ($)

-- Addition of two values
add :: Integer -> Integer -> ErrM Integer
add = (return .) . (+)