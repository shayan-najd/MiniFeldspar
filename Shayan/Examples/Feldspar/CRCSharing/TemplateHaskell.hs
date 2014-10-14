
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.CRC.Common

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

crc32 :: Data (Ary Integer -> Integer)
crc32 = [|| $$foldl $$updCrc 0 ||]

updCrc :: Data (Integer -> Integer -> Integer)
updCrc = [|| \ cc -> \ ch ->
             $$bitXor
             ($$bitXor
              (ind $$tbl
               ($$bitAnd ($$bitXor ($$bitXor cc 0xFFFFFFFF) ch) 0xff))
              ($$shfRgt ($$bitXor cc 0xFFFFFFFF) 8))
             0xFFFFFFFF ||]

tbl :: Data (Ary Integer)
tbl = hashTable

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

crc32FMWS :: FMWS.Exp (TFA.Ary TFA.Int ': Prelude) TFA.Int
crc32FMWS = MP.frmRgt (cnv ([|| $$crc32 dummyAry0 ||]
                                , TFG.Ary TFG.Int <:> etTFG
                                , 'dummyAry0 <+> esTH))

main :: MP.IO ()
main = let f = MP.frmRgt
               (scompileWith [("v0" , TFA.Ary TFA.Int)]
                TFG.Int
                ("v0" <+> esString) 1
                (nrm (cse crc32FMWS)))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile "CRCTemplateHaskell.c" f'