

import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
--import Examples.Feldspar.CRC.Common

import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue       as FGV
import qualified Type.Feldspar.GADT                  as TFG
import Compiler (scompileWith)

import Normalization
import Normalization.Feldspar.MiniWellScoped ()

import qualified Language.Haskell.TH.Syntax          as TH
import qualified Expression.Feldspar.MiniWellScoped  as FMWS
import qualified Type.Feldspar.ADT                   as TFA
import Conversion.Expression.Feldspar ()

import qualified Environment.Scoped                  as ES
import qualified Environment.Typed                   as ET
import Singleton (Len)
import qualified Nat.ADT                             as NA

import Optimization
import Optimization.Feldspar.MiniWellScoped ()

unit :: Data (Ary Float)
unit =  [|| ary 1 (\ _i -> 0.0) ||]

geometric :: Data (Float -> Float -> Float)
geometric =  [|| \ x -> \ y -> $$sqrt ($$((*)) x y) ||]

append :: Data (Ary t -> Ary t -> Ary t)
append =  [|| \ a -> \ b -> 
              ary 
              ($$((+)) (len a) (len b)) 
              (\ i -> if $$((<)) i (len a) 
                      then ind a i 
                      else ind b ($$((-)) i (len a))) ||]

blur :: Data (Ary Float -> Ary Float)
blur =  [|| \ a -> $$zipWith $$geometric 
                   ($$append $$unit a) 
                   ($$append a $$unit) ||]

size :: Data Int
size = [|| 100000 ||]

-- test :: Data Float
test   = [|| $$sum ($$blur ($$((...)) 0 $$size)) ||]

test2 :: Data Float
test2  = [|| $$sum ($$blur ($$blur ($$((...)) 0 $$size))) ||]

test2m :: Data Float
test2m = [|| $$sum ($$blur ($$memorize ($$blur ($$((...)) 0 $$size)))) ||]


testFMWS :: FMWS.Exp Prelude TFA.Flt
testFMWS = opt (MP.frmRgt (cnv (test , etTFG , esTH))) etFGV

main :: MP.IO ()
main = let f = MP.frmRgt (scompileWith [] TFG.Flt esString 0 testFMWS) 
       in  MP.putStrLn f


{-
 
   
dummyAry0 :: Ary Integer
dummyAry0 = dummyAry0

es :: ES.Env ((NA.Suc (Len Prelude))) TH.Name
es = 'dummyAry0 <+> esTH      

et :: ET.Env TFG.Typ (TFA.Ary TFA.Int ': Prelude)
et = TFG.Ary TFG.Int <:> etTFG
 
crc32FMWS :: FMWS.Exp (TFA.Ary TFA.Int ': Prelude) 
             TFA.Int
crc32FMWS = nrm (MP.frmRgt (cnv ([|| $$crc32 dummyAry0 ||] , et , es)))

main :: MP.IO ()
main = let f = MP.frmRgt (scompileWith [("v0" , TFA.Ary TFA.Int)]  
                          TFG.Int ("v0" <+> esString) 1 crc32FMWS) 
           in  MP.writeFile "CRCTemplateHaskell.c" f
-}