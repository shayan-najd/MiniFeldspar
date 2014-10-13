
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.Windowing.Common

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

windowing :: Data (Ary Complex -> Ary Complex)
windowing = [|| \ s -> let l = len s
                       in  $$zipWith $$mul
                               ($$append
                                ($$replicate ($$sub l ($$div l 4))
                                               (cmx 1.0 0.0))
                                ($$replicate l (cmx 0.0 0.0))) s ||]

inp :: Data (Ary Complex)
inp = fromList (MP.fmap (\ i -> [|| cmx i 0.0 ||]) tstPGM) [|| cmx 0.0 0.0 ||]

out :: [MP.Float]
out  = let outFMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Cmx) =
             MP.frmRgt (cnv ([|| $$windowing $$inp ||] , etTFG , esTH ))
           (FGV.Exp e) :: FGV.Exp (TFA.Ary TFA.Cmx) =
             MP.frmRgt (cnv (outFMWS , etFGV))
       in MP.fmap MP.realPart (MP.elems e)

prop :: MP.Bool
prop = out MP.== tstPGMW

dummyVec :: Ary Complex
dummyVec = dummyVec

windowingBWFMWS :: FMWS.Exp (TFA.Ary TFA.Cmx ': Prelude) (TFA.Ary TFA.Cmx)
windowingBWFMWS = MP.frmRgt
                  (cnv ([|| $$windowing dummyVec ||]
                       , TFG.Ary TFG.Cmx <:> etTFG
                       , 'dummyVec <+> esTH))

main :: MP.IO ()
main = let f = MP.frmRgt
               (scompileWith [("v0" , TFA.Ary TFA.Cmx)]
                (TFG.Ary TFG.Cmx)
                ("v0" <+> esString) 1
                (nrm windowingBWFMWS))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile "WindowingTemplateHaskell.c" f'
