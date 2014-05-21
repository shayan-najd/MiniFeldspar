

import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment

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

upto :: Data (Integer -> Ary Float)
upto =  [|| \ m -> ary m (\ i -> $$i2f i) ||]

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
blur =  [|| \ a -> $$zipWith 
                   $$geometric 
                   ($$append $$unit a) 
                   ($$append a $$unit) ||]

size :: Data Int
size = [|| 100000 ||]

test :: Data Float
test   = [|| $$sum ($$blur ($$upto $$size)) ||]

test2 :: Data Float
test2  = [|| $$sum ($$blur ($$blur ($$upto $$size))) ||]

test2m :: Data Float
test2m = [|| $$sum ($$blur ($$memorize ($$blur ($$upto $$size)))) ||]

test3 :: Data (Ary Float)
test3 = [|| $$blur ($$blur ($$upto $$size)) ||]

test3m :: Data (Ary Float)
test3m = [|| $$blur($$memorize ($$blur ($$upto $$size))) ||]

testFMWS :: FMWS.Exp Prelude TFA.Flt
testFMWS = opt (MP.frmRgt (cnv (test , etTFG , esTH))) etFGV

test2FMWS :: FMWS.Exp Prelude TFA.Flt
test2FMWS = opt (MP.frmRgt (cnv (test2 , etTFG , esTH))) etFGV

test2mFMWS :: FMWS.Exp Prelude TFA.Flt
test2mFMWS = opt (MP.frmRgt (cnv (test2m , etTFG , esTH))) etFGV

test3FMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Flt)
test3FMWS = opt (MP.frmRgt (cnv (test3 , etTFG , esTH))) etFGV

test3mFMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Flt)
test3mFMWS = opt (MP.frmRgt (cnv (test3m , etTFG , esTH))) etFGV

testC :: MP.String
testC = MP.frmRgt (scompileWith [] TFG.Flt esString 0 testFMWS) 

test2C :: MP.String
test2C = MP.frmRgt (scompileWith [] TFG.Flt esString 0 test2FMWS) 

test2mC :: MP.String
test2mC = MP.frmRgt (scompileWith [] TFG.Flt esString 0 test2mFMWS) 

test3C :: MP.String
test3C = MP.frmRgt (scompileWith [] TFG.Flt esString 0 test3FMWS) 

test3mC :: MP.String
test3mC = MP.frmRgt (scompileWith [] TFG.Flt esString 0 test3mFMWS)  