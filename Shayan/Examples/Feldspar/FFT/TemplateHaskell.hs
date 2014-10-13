
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.FFT.Common

import Conversion
import Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue       as FGV
import qualified Type.Feldspar.GADT                  as TFG
import Compiler (scompileWith)

import qualified Expression.Feldspar.MiniWellScoped  as FMWS
import qualified Type.Feldspar.ADT                   as TFA
import Expression.Feldspar.Conversion ()

import Normalization
import Normalization.Feldspar.MiniWellscoped ()

fft :: Data (Ary Complex -> Ary Complex)
fft = [|| \ v ->
           let steps = $$sub ($$ilog2 (len v)) 1 in
           $$bitRev steps ($$fftCore steps v)
      ||]

fftCore :: Data (Integer -> Ary Complex -> Ary Complex)
fftCore = [|| \ n -> \ vv ->
              $$forLoop ($$add n 1) vv
                      (\ j -> \ v ->
                              ary (len vv) (\ i -> $$ixf v ($$sub n j) i))
          ||]

ixf :: Data (Ary Complex -> Integer -> Integer -> Complex)
ixf = [|| \ v -> \ k -> \ i ->
          let k2   = $$shfLft 1 k in
          let twid = $$cis ($$div ($$mul $$pi ($$i2f ($$lsbs k i)))($$i2f k2)) in
          let a    = ind v i in
          let b    = ind v ($$bitXor i k2) in
            if $$testBit i k
            then $$mul twid ($$sub b a)
            else $$add a b
      ||]

bitRev :: Data (Integer -> Ary Complex -> Ary Complex)
bitRev = [|| \ n -> \ x ->
             $$forLoop n x (\ i -> $$permute (\ _j -> $$rotBit ($$add i 1)))
         ||]

rotBit :: Data (Integer -> Integer -> Integer)
rotBit = [|| \ k -> \ i ->
          $$bitOr
          ($$shfLft ($$bitOr
                     ($$shfLft ($$shfRgt ($$shfRgt i 1) k) 1)
                     ($$bitAnd i 1)) k)
          ($$lsbs k ($$shfRgt i 1))
         ||]

inp :: Data (Ary Complex)
inp = fromList (MP.fmap (\ f -> [|| cmx f 0.0 ||]) tstInp)
      [|| cmx 0.0 0.0 ||]

out :: [MP.Float]
out  = let outFMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Cmx) =
             MP.frmRgt (cnv ([|| $$fft $$inp ||] , etTFG , esTH))
           (FGV.Exp e) :: FGV.Exp (TFA.Ary TFA.Cmx) =
             MP.frmRgt (cnv (outFMWS , etFGV))
       in MP.fmap MP.magnitude (MP.elems e)

prop :: MP.Bool
prop = test out

dummyAry :: Ary Complex
dummyAry = dummyAry

fftFMWS :: FMWS.Exp (TFA.Ary TFA.Cmx ': Prelude) (TFA.Ary TFA.Cmx)
fftFMWS = MP.frmRgt (cnv ([|| $$fft dummyAry  ||]
                         , TFG.Ary TFG.Cmx <:> etTFG
                         , 'dummyAry <+> esTH))

main :: MP.IO ()
main = let f = MP.frmRgt
               (scompileWith [("v0" , TFA.Ary TFA.Cmx)]
                (TFG.Ary TFG.Cmx)
                ("v0" <+> esString) 1
                (nrm fftFMWS))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in MP.writeFile "FFTTemplateHaskell.c" f'
