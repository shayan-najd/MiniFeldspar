

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

unit :: Data (t -> Ary t)
unit =  [|| \ x -> ary 1 (\ _i -> x) ||]

sqr :: Data (Float -> Float)
sqr = [|| \ x -> $$((*)) x x ||] 

geometric :: Data (Float -> Float -> Float)
geometric =  [|| \ x y -> $$sqrt ($$((*)) ($$sqr x) ($$sqr y)) ||]

append :: Data (Ary t -> Ary t -> Ary t)
append =  [|| \ a b -> ary 
                       ($$((+)) (len a) (len b)) 
                       (\ i -> if $$((<)) i (len a) 
                               then ind a i 
                               else ind b ($$((-)) i (len a))) ||]

blur :: Data (Ary Float -> Ary Float)
blur =  [|| \ a -> $$zipWith $$geometric 
                   ($$append ($$unit 0) a) 
                   ($$append a ($$unit 0)) ||]

size :: Data Int
size = [|| 100000 ||]

test :: Data Float
test =  [|| $$sum ($$blur ($$((...)) 0 $$size)) ||]

test2 :: Data Float
test2 =  [|| $$sum ($$blur ($$blur ($$((...)) 0 $$size))) ||]

test2m :: Data Float
test2m =  [|| $$sum ($$blur ($$memorize ($$blur ($$((...)) 0 $$size)))) ||]


{-
crc32 :: Data (Ary Integer -> Integer)
crc32 = [|| $$foldl $$updCrc 0 ||]
 
updCrc :: Data (Integer -> Integer -> Integer)
updCrc = [|| \ cc -> \ ch -> 
             $$xor 
             ($$xor 
              (ind $$tbl 
               ($$((.&.))
                ($$xor ($$xor cc 0xFFFFFFFF) ch) 0xff))
              ($$((.>>.)) ($$xor cc 0xFFFFFFFF)  8)) 
             0xFFFFFFFF ||]
          
tbl :: Data (Ary Integer)
tbl = fromList (MP.fmap (\ i -> [|| i ||]) tblLst)  [|| 0 ||]                 

inp :: Data (Ary Integer)
inp = fromList (MP.fmap (\ i -> [|| i ||]) tstInp) [|| 0 ||]
      
out :: MP.Integer
out  = let outFMWS :: FMWS.Exp Prelude TFA.Int =
             MP.frmRgt (cnv ([|| $$crc32 $$inp ||] , etTFG , esTH ))
           (FGV.Exp e) :: FGV.Exp TFA.Int =  MP.frmRgt (cnv (outFMWS , etFGV)) 
       in e    

prop :: MP.Bool
prop = test out 
  
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