

import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell 
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.IP.Common

import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue       as FGV
import qualified Type.Feldspar.GADT                  as TFG
import Compiler (scompileWith)

import Normalization
import Normalization.Feldspar.MiniWellScoped ()

import qualified Expression.Feldspar.MiniWellScoped  as FMWS
import qualified Type.Feldspar.ADT                   as TFA
import Conversion.Expression.Feldspar ()
 
 
toBW :: Data (Ary Integer -> Ary Integer)
toBW = [|| $$map (\ x -> if $$((<)) x 135 then 1 else 0) ||]

redCoefficient :: Data Integer
redCoefficient   = [|| 30 ||]

greenCoefficient :: Data Integer
greenCoefficient = [|| 59 ||]

blueCoefficient :: Data Integer
blueCoefficient  = [|| 11 ||]

rgbToGray :: Data (Integer -> Integer -> Integer -> Integer)
rgbToGray = [|| \ r -> \ g -> \ b ->  
               $$((/)) 
               ($$((+))
                ($$((+)) ($$((*)) r $$redCoefficient )
                         ($$((*)) g $$greenCoefficient))
                ($$((*)) b $$blueCoefficient )) 100 ||]
                 
toGray :: Data (Ary Integer -> Ary Integer)
toGray = [|| \ v -> ary ($$((/)) (len v) 3)
                    (\ i -> let j = $$((*)) i 3
                            in $$rgbToGray 
                               (ind v j) 
                               (ind v ($$((+)) j 1)) 
                               (ind v ($$((+)) j 2))) ||]
 
fromColoredtoBW :: Data (Ary Integer -> Ary Integer)
fromColoredtoBW = [|| \ v -> $$toBW ($$toGray v) ||]

inp :: Data (Ary Integer)
inp = fromList (MP.fmap (\ i -> [|| i ||]) tstPPM) [|| 0 ||]
      
out :: [MP.Integer]
out  = let outFMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Int) =
             MP.frmRgt (cnv ([|| $$fromColoredtoBW $$inp ||] , etTFG , esTH ))
           (FGV.Exp e) :: FGV.Exp (TFA.Ary TFA.Int) = 
             MP.frmRgt (cnv (outFMWS , etFGV)) 
       in MP.elems e    

prop :: MP.Bool
prop = out MP.== tstPBM

dummyVec :: Ary Integer
dummyVec = dummyVec
 
fromColoredtoBWFMWS :: FMWS.Exp (TFA.Ary TFA.Int ': Prelude) (TFA.Ary TFA.Int)
fromColoredtoBWFMWS = MP.frmRgt 
                           (cnv ([|| $$fromColoredtoBW dummyVec ||] 
                                , TFG.Ary TFG.Int <:> etTFG 
                                , 'dummyVec <+> esTH))

main :: MP.IO ()
main = MP.getArgs MP.>>=  
       (\ [as] -> let f = MP.frmRgt 
                          (scompileWith [("v0" , TFA.Ary TFA.Int)]  
                           (TFG.Ary TFG.Int) 
                           ("v0" <+> esString) 1 
                           (nrmIf (as MP./= "NoNrm") fromColoredtoBWFMWS)) 
                      f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC    
                  in  MP.writeFile (as MP.++ "IPTemplateHaskell.c") f')     