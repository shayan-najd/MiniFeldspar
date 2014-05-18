{-# LANGUAGE RebindableSyntax #-}
 
import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.FFT.Common

import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG
import Compiler(scompile)

import Normalization
import Normalization.Feldspar.MiniWellScoped ()

fft :: Vec Complex -> Vec Complex
fft v = let steps = ilog2 (length v) - 1 
        in  bitRev steps (fftCore steps v)
         
fftCore :: Data Integer -> Vec Complex -> Vec Complex
fftCore n vv = forLoopVec (n + 1) vv 
               (\ j v -> vec (length vv) (ixf v (n - j)))

ixf :: Vec Complex
    -> Data Integer -> Data Integer -> Data Complex
ixf v k i = let k2   = 1 .<<. k
                a    = v !!  i
                b    = v !! (xor i k2)
                twid = cis (litF (MP.negate MP.pi) 
                            * i2f (lsbs k i) / i2f k2)
            in  if testBit i k then twid * (b - a) else a + b
          
bitRev :: Data Integer -> Vec Complex -> Vec Complex
bitRev n x = forLoopVec n x (\ i -> permute (\ _ -> rotBit (i + 1)))
 
rotBit :: Data Integer -> Data Integer -> Data Integer
rotBit k i = lefts .|. rights
  where
    ir     = i .>>. 1
    rights = lsbs k ir
    lefts  = (((ir .>>. k) .<<. 1) .|. (i .&. 1)) .<<. k
  
inp :: Vec Complex
inp = fromList (MP.fmap (\ f -> cmx (litF f) 0.0) tstInp) 
      (cmx 0.0 0.0)
 
out :: [MP.Float]
out = let FGV.Exp e = MP.frmRgt (cnv (vec2ary (fft inp) , etFGV)) 
                      :: FGV.Exp (Ary Complex) 
      in MP.fmap MP.magnitude (MP.elems e)
 
prop :: MP.Bool
prop = test out

main :: MP.IO ()
main = let f = MP.frmRgt (scompile (TFG.Ary TFG.Cmx) esString 
                          (nrm (vec2ary MP.. fft MP.. ary2vec)))
       in MP.writeFile "FFTMiniWellScoped.c" f    