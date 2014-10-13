
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.IPBW.Common

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
import CSE

toBW :: Data (Ary Integer -> Ary Integer)
toBW = [|| $$map (\ x -> if $$lt x 135 then 1 else 0) ||]

inp :: Data (Ary Integer)
inp = fromList (MP.fmap (\ i -> [|| i ||]) tstPPM) [|| 0 ||]

out :: [MP.Integer]
out  = let outFMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Int) =
             MP.frmRgt (cnv ([|| $$toBW $$inp ||] , etTFG , esTH ))
           (FGV.Exp e) :: FGV.Exp (TFA.Ary TFA.Int) =
             MP.frmRgt (cnv (outFMWS , etFGV))
       in MP.elems e

prop :: MP.Bool
prop = out MP.== tstPBM

dummyVec :: Ary Integer
dummyVec = dummyVec

toBWFMWS :: FMWS.Exp (TFA.Ary TFA.Int ': Prelude) (TFA.Ary TFA.Int)
toBWFMWS = MP.frmRgt
           (cnv ([|| $$toBW dummyVec ||]
                , TFG.Ary TFG.Int <:> etTFG
                , 'dummyVec <+> esTH))

main :: MP.IO ()
main = let f = MP.frmRgt
               (scompileWith [("v0" , TFA.Ary TFA.Int)]
                (TFG.Ary TFG.Int)
                ("v0" <+> esString) 1
                (nrm (cse toBWFMWS)))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile "IPBWTemplateHaskell.c" f'
