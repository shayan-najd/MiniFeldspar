{-# LANGUAGE RebindableSyntax #-}
module Examples.Feldspar.FFT.Feldspar where

import Prelude ()
import qualified MyPrelude as MP
import Examples.Feldspar.Prelude.Feldspar

import Feldspar(eval)
import Feldspar.Compiler 
import qualified Data.Complex as CMPX

fft :: Vec Complex -> Vec Complex
fft v = let steps = ilog2 (length v) - 1 
        in  bitRev steps (fftCore steps v)
  
fftCore :: Data Integer -> Vec Complex -> Vec Complex
fftCore n vv = forLoop (n + 1) vv 
               (\ j v -> vec (length vv) (ixf v (n - j)))
 
ixf :: Vec Complex
             -> Data Integer -> Data Integer -> Data Complex
ixf v k i = let k2   = 1 .<<. k
                a    = v !! i
                b    = v !! (xor i k2)
                twid = cis (litF (MP.negate MP.pi)
                            * i2f (lsbs k i) / i2f k2)
             in if (testBit i k) then (twid * (b - a)) else (a + b)
          
bitRev :: Data Integer -> Vec Complex -> Vec Complex
bitRev n x = forLoop n x (\ i -> permute (\ _ -> rotBit (i + 1)))
 
rotBit :: Data Integer -> Data Integer -> Data Integer
rotBit k i = lefts .|. rights
  where
    ir     = i .>>. 1
    rights = lsbs k ir
    lefts  = (((ir .>>. k) .<<. 1) .|. (i .&. 1)) .<<. k

prop :: MP.Bool
prop = out MP.== 
       [6.2831855,16.418755,8.885766,6.800871
       ,6.2831855,6.8008704,8.885766,16.418755]


lst :: [Data Float]
lst = [-4.71238898038469,
       -3.141592653589793,
       -1.5707963267948966,
       0.0,
       1.5707963267948966,
       3.141592653589793,
       4.71238898038469,
       6.283185307179586]

inp :: Vec Complex
inp = fromList (MP.fmap 
                (\ f -> complex f 0.0) lst) (complex 0.0 0.0)
                         
out :: [Float]
out = MP.fmap CMPX.magnitude (MP.snd (eval (fft inp)))

fftC :: MP.IO ()
fftC = icompileWith (defaultOptions{printHeader = MP.True}) fft

