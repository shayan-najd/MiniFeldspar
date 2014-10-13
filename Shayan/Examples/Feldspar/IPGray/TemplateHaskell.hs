
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.IPGray.Common

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

redCoefficient :: Data Integer
redCoefficient   = [|| 30 ||]

greenCoefficient :: Data Integer
greenCoefficient = [|| 59 ||]

blueCoefficient :: Data Integer
blueCoefficient  = [|| 11 ||]

rgbToGray :: Data (Integer -> Integer -> Integer -> Integer)
rgbToGray = [|| \ r -> \ g -> \ b ->
               $$div
               ($$add
                ($$add ($$mul r $$redCoefficient )
                         ($$mul g $$greenCoefficient))
                ($$mul b $$blueCoefficient )) 100
            ||]

toGray :: Data (Ary Integer -> Ary Integer)
toGray = [|| \ v ->
              ary ($$div (len v) 3)
                      (\ i -> let j = $$mul i 3 in
                              $$rgbToGray
                              (ind v j)
                              (ind v ($$add j 1))
                              (ind v ($$add j 2)))
         ||]

inp :: Data (Ary Integer)
inp = fromList (MP.fmap (\ i -> [|| i ||]) tstPPM) [|| 0 ||]

out :: [MP.Integer]
out  = let outFMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Int) =
             MP.frmRgt (cnv ([|| $$toGray $$inp ||] , etTFG , esTH ))
           (FGV.Exp e) :: FGV.Exp (TFA.Ary TFA.Int) =
             MP.frmRgt (cnv (outFMWS , etFGV))
       in MP.elems e

prop :: MP.Bool
prop = out MP.== tstPGM

dummyVec :: Ary Integer
dummyVec = dummyVec

fromColoredtoBWFMWS :: FMWS.Exp (TFA.Ary TFA.Int ': Prelude) (TFA.Ary TFA.Int)
fromColoredtoBWFMWS = MP.frmRgt
                           (cnv ([|| $$toGray dummyVec ||]
                                , TFG.Ary TFG.Int <:> etTFG
                                , 'dummyVec <+> esTH))

main :: MP.IO ()
main = let f = MP.frmRgt
               (scompileWith [("v0" , TFA.Ary TFA.Int)]
                (TFG.Ary TFG.Int)
                ("v0" <+> esString) 1
                (nrm fromColoredtoBWFMWS))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile "IPGrayTemplateHaskell.c" f'
