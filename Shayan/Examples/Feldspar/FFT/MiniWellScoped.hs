{-# LANGUAGE RebindableSyntax #-}

import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.FFT.Common

import Conversion
import Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG
import Compiler(scompile)

import Normalization
import Normalization.Feldspar.MiniWellscoped ()

fft :: Vec Complex -> Vec Complex
fft = \ v ->
      let steps = sub (ilog2 (lenV v)) 1 in
      bitRev steps (fftCore steps v)

fftCore :: Data Integer -> Vec Complex -> Vec Complex
fftCore = \ n -> \ vv ->
          forLoopVec (add n 1) vv
                (\ j -> \ v ->
                        vec (lenV vv) (\ i -> ixf v (sub n j) i))

ixf :: Vec Complex
    -> Data Integer -> Data Integer -> Data Complex
ixf = \ v -> \ k -> \ i ->
      let k2   = shfLft 1 k in
      let twid = cis (div (mul pi (i2f (lsbs k i))) (i2f k2)) in
      let a    = indV v i in
      let b    = indV v (bitXor i k2) in
        if testBit i k
        then mul twid (sub b a)
        else add a b

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
main = let f = MP.frmRgt
               (scompile
                (TFG.Ary TFG.Cmx)
                esString
                (nrm fftAry))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile "FFTMiniWellScoped.c" f'