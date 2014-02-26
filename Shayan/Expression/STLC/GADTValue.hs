{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Expression.STLC.GADTValue where

import ErrorMonad

var :: t -> t
var = id

con :: Integer -> Integer
con = id

abs :: (a -> ErrM b) -> (a -> b)
abs = (frmRgt .) 

-- Application of two values
app :: (a -> b) -> a -> b
app = ($)

-- Addition of two values
add :: Integer -> Integer -> Integer
add = (+)