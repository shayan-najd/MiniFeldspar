{-# LANGUAGE RebindableSyntax #-}

import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.FFT.Common

import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG
import Compiler(scompile)

fft :: Vec Complex -> Vec Complex
fft = \ v -> (\ steps -> bitRev steps (fftCore steps v))
      (sub (ilog2 (lenV v)) 1)

fftCore :: Data Integer -> Vec Complex -> Vec Complex
fftCore = \ n -> \ vv -> forLoopVec (add n 1) vv
          (\ j -> \ v -> vec (lenV vv)
                  (ixf v (sub n j)))

ixf :: Vec Complex
    -> Data Integer -> Data Integer -> Data Complex
ixf = let p = litF (MP.negate MP.pi) in
    \ v -> \ k -> \ i ->
      (\ k2 -> (\ twid -> \ a -> \ b ->
                if testBit i k
                then mul twid (sub b a)
                else add a b)
       (cis (div (mul p (i2f (lsbs k i))) (i2f k2)))
       (indV v i)
       (indV v (bitXor i k2)))
    (shfLft 1 k)

bitRev :: Data Integer -> Vec Complex -> Vec Complex
bitRev = \ n -> \ x ->
         forLoopVec n x (\ i -> permute (\ _j -> rotBit (add i 1)))

rotBit :: Data Integer -> Data Integer -> Data Integer
rotBit = \ k -> \ i ->
         bitOr
         (shfLft (bitOr
                  (shfLft (shfRgt (shfRgt i 1) k) 1)
                  (bitAnd i 1)) k)
         (lsbs k (shfRgt i 1))

inp :: Vec Complex
inp = fromList (MP.fmap (\ f -> cmx (litF f) 0.0) tstInp)
      (cmx 0.0 0.0)

out :: [MP.Float]
out = let FGV.Exp e = MP.frmRgt (cnv (vec2ary (fft inp) , etFGV))
                      :: FGV.Exp (Ary Complex)
      in MP.fmap MP.magnitude (MP.elems e)

prop :: MP.Bool
prop = test out

fftAry :: Data (Ary Complex) -> Data (Ary Complex)
fftAry = vec2ary MP.. fft MP.. ary2vec

main :: MP.IO ()
main = MP.getArgs MP.>>=
       (\ [as] -> let f = MP.frmRgt
                          (scompile
                           (TFG.Ary TFG.Cmx)
                           esString
                           ({- nrmIf (as MP./= "NoNrm") -} fftAry))
                      f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
                  in  MP.writeFile (as MP.++ "FFTMiniWellScoped.c") f')