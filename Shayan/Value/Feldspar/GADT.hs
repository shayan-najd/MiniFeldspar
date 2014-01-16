{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Value.Feldspar.GADT where

import ErrorMonad
import Data.Array

conI :: Integer -> ErrM Integer
conI = return
     
conB :: Bool -> ErrM Bool
conB = return

abs :: (a -> ErrM b) -> ErrM (a -> b)
abs f = return (\ va -> case f va of 
                   Rgt vb -> vb
                   Lft s  -> error s) 
        
-- Application of two values
app :: (ta -> tb) -> ta -> ErrM tb
app = (return .) . ($)

addV :: Integer -> Integer -> Integer
addV = (+)
 
cnd :: Bool -> a -> a -> ErrM a
cnd vc vt vf = return (if vc then vt else vf)

fst :: (a , b) -> ErrM a
fst (vf , _ ) = return vf

snd :: (a , b) -> ErrM b
snd (_  , vs) = return vs

tpl :: a -> b -> ErrM (a , b)
tpl = (return .) . ((,)) 
 
arr :: Integer -> (Integer -> a) -> ErrM (Array Integer a)
arr l vf = return (listArray (0 , l)  
                   [vf i | i <- [0 .. l]])

len :: Array Integer a -> ErrM Integer
len a = (return . (1 +) . uncurry (flip (-)) . bounds) a

ind :: Array Integer a -> Integer -> ErrM a
ind a i = return (a ! i)

whl :: (s -> Bool) -> (s -> s) -> s -> ErrM s
whl fc fb v = return (head (dropWhile fc (iterate fb v)))