{-# LANGUAGE RebindableSyntax #-}
module Examples.Feldspar.FFT.Feldspar where

import Prelude ()
import qualified MyPrelude as MP
import Examples.Feldspar.Prelude.Feldspar

import Feldspar(eval)
import Feldspar.Compiler 
import qualified Data.Complex as CMPX

fft :: Vec Complex -> Vec Complex
fft  =  \ v -> forLoop (length v) v
             (\ _i -> 
               \ vv -> vec (length v) (\ i -> vv !! i))
   
fft' :: Ary Complex -> Ary Complex
fft' = \ v -> forLoop (length v) v
             (\ _i -> 
               \ vv -> vec (length v) (\ i -> vv !! i))

fftC :: MP.IO ()
fftC = icompile fft

