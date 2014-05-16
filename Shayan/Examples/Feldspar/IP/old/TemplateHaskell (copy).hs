-- module Examples.Feldspar.IP.TemplateHaskell where

import Prelude (IO)
import qualified MyPrelude as MP
import Examples.Feldspar.Prelude.TemplateHaskell 
import Examples.Feldspar.Prelude.Environment
import Conversion
import Conversion.Expression.Feldspar ()
import Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed ()
import qualified Language.Haskell.TH.Syntax as TH
import qualified Expression.Feldspar.ADTUntypedNamed as FAUN
import qualified Expression.Feldspar.ADTValue        as FAV
import qualified Expression.Feldspar.GADTHigherOrder as FGHO
import qualified Expression.Feldspar.MiniWellScoped  as FMWS
import qualified Type.Feldspar.ADT                   as TFA
import qualified Type.Feldspar.GADT                  as TFG
import qualified Environment.Scoped                  as ES
import qualified Environment.Typed                   as ET
import Normalization
import Normalization.Feldspar.GADTHigherOrder ()
import Compiler (scompileWith)
import qualified Nat.ADT                             as NA
import Singleton (Len)
--import qualified Expression.Feldspar.GADTValue as FGV
--import Feldspar.Compiler (icompile)
 
-- Conversion from grayscale to black & white 
toBW :: Data (Vec Integer -> Vec Integer)
toBW = [|| $$map (\ x -> if $$((<)) x 135 then 1 else 0) ||]

-- The standard red channel grayscale coefficient
redCoefficient :: Data Integer
redCoefficient   = [|| 30 ||]

-- The standard green channel grayscale coefficient
greenCoefficient :: Data Integer
greenCoefficient = [|| 59 ||]

-- The standard blue channel grayscale coefficient
blueCoefficient :: Data Integer
blueCoefficient  = [|| 11 ||]

-- Conversion from RGB to grayscale
rgbToGray :: Data (Integer -> Integer -> Integer -> Integer)
rgbToGray = [|| \ r -> \ g -> \ b ->  
               $$((/)) 
               ($$((+))
                ($$((+)) ($$((*)) r $$redCoefficient )
                         ($$((*)) g $$greenCoefficient))
                ($$((*)) b $$blueCoefficient )) 100 ||]
                 
-- Conversion from colored to grayscale       
toGray :: Data (Vec Integer -> Vec Integer)
toGray = [|| \ v -> $$vec ($$((/)) ($$length v) 3)
                    (\ i -> let j = $$((*)) i 3
                            in $$rgbToGray 
                               ($$((!!)) v j) 
                               ($$((!!)) v ($$((+)) j 1)) 
                               ($$((!!)) v ($$((+)) j 2))) ||]
 
-- Conversion from colored to black and white
fromColoredtoBW :: Data (Vec Integer -> Vec Integer)
fromColoredtoBW = [|| \ v -> $$toBW ($$toGray v) ||]

toGray' :: Data (Ary Integer -> Ary Integer)
toGray' = [|| \ a -> $$vec2ary ($$toGray ($$ary2vec a)) ||]

dummyVec :: Ary Integer
dummyVec = dummyVec

es :: ES.Env (NA.Suc (Len Prelude)) TH.Name
es = 'dummyVec <+> esTH      

et :: ET.Env TFG.Typ (TFA.Ary TFA.Int ': Prelude)
et = TFG.Ary TFG.Int <:> etTFG

toGrayFGHO :: FGHO.Exp (TFA.Ary TFA.Int ': Prelude) (TFA.Ary TFA.Int)
toGrayFGHO = nrm (MP.frmRgt (cnv ([|| $$toGray' dummyVec ||] , et , es)))

toGrayFMWS :: FMWS.Exp (TFA.Ary TFA.Int ': Prelude) (TFA.Ary TFA.Int)
toGrayFMWS = MP.frmRgt (cnv (toGrayFGHO , et , es)) 

main :: MP.IO ()
main = let f = MP.frmRgt (scompileWith [("v0" , TFA.Ary TFA.Int)]  
                             (TFG.Ary TFG.Int) ("v0" <+> esString) 1 toGrayFMWS) 
       in MP.writeFile "/home/tester/TH.c" f 

{-
--toGrayC :: IO ()
--toGrayC = icompile toGray
 
main :: IO ()
main = do let filePPM = "Examples/Feldspar/IP/Image/Lena/Image3.ppm"
          let filePGM = "Examples/Feldspar/IP/Image/Lena/Image3.pgm"
          f <- MP.readFile filePPM
          let "P3" : s : "255" : c = MP.lines f
              inp :: [Data Integer]
              inp = MP.fmap (litI MP.. (MP.read :: MP.String -> MP.Integer)) c
              inpVec = fromList inp [|| 0 ||] 
              inpAry = [|| $$vec2ary $$inpVec ||]
              out    = [|| $$toGray' $$inpAry ||]
              outFAUN :: FAUN.Exp TH.Name = MP.frmRgt (cnv (out , etTFG , esTH ))
              res :: FAV.Exp = MP.frmRgt (cnv (outFAUN , emTHFAV))
              fres :: Ary Integer = FAV.colft res
              frs = MP.elems fres
              v' = MP.unlines ("P2" : s : "255" : MP.fmap MP.show frs)
          MP.writeFile filePGM  v' -}