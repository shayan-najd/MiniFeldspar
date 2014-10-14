module Examples.Feldspar.Prelude.Environment where

import MyPrelude hiding (Int)

import qualified Expression.Feldspar.ADTValue  as FAV
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Language.Haskell.TH.Syntax    as TH

import qualified Nat.ADT            as NA
import Nat.TH   as NT

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import qualified Environment.Map    as EM
import qualified Environment.Plain  as EP
import qualified Environment.Scoped as ES
import qualified Environment.Typed  as ET

import Conversion
import Environment.Conversion ()

import Variable.Typed
import Singleton

infixr 9 :->
type ta :-> tb = TFA.Arr ta tb

(-->) :: TFG.Typ a -> TFG.Typ b -> TFG.Typ (a :-> b)
(-->) = TFG.Arr

infixr 5 <:>
(<:>) :: tf t -> ET.Env tf e -> ET.Env tf (t ': e)
(<:>) = ET.Ext

infixr 5 <+>
(<+>) :: t -> ES.Env n t -> ES.Env (NA.Suc n) t
(<+>) = ES.Ext

type Prelude = TFA.Cmx :-> TFA.Flt ':
               TFA.Cmx :-> TFA.Flt ':
               TFA.Bol :-> (TFA.Bol :-> TFA.Bol) ':
               TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
               TFA.Flt :-> (TFA.Flt :-> TFA.Bol) ':
               TFA.Bol :-> (TFA.Bol :-> TFA.Bol) ':
               TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
               TFA.Flt :-> (TFA.Flt :-> TFA.Bol) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Flt :-> (TFA.Flt :-> TFA.Flt) ':
               TFA.Flt :-> (TFA.Flt :-> TFA.Flt) ':
               TFA.Flt :-> (TFA.Flt :-> TFA.Flt) ':
               TFA.Flt :-> (TFA.Flt :-> TFA.Flt) ':
               TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx) ':
               TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx) ':
               TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx) ':
               TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> TFA.Int               ':
               TFA.Int :-> TFA.Flt               ':
               TFA.Flt :-> TFA.Cmx               ':
               TFA.Int :-> TFA.Int               ':
               TFA.Flt :-> TFA.Flt               ':
               TFA.Ary TFA.Flt :-> TFA.Ary TFA.Flt ':
               TFA.Ary TFA.Int                   ':
               '[]

---------------------------------------------------------------------------------
-- ETTFG
---------------------------------------------------------------------------------

etTFG :: ET.Env TFG.Typ Prelude
etTFG = TFG.Cmx --> TFG.Flt <:>
        TFG.Cmx --> TFG.Flt <:>
        TFG.Bol --> (TFG.Bol --> TFG.Bol) <:>
        TFG.Int --> (TFG.Int --> TFG.Bol) <:>
        TFG.Flt --> (TFG.Flt --> TFG.Bol) <:>
        TFG.Bol --> (TFG.Bol --> TFG.Bol) <:>
        TFG.Int --> (TFG.Int --> TFG.Bol) <:>
        TFG.Flt --> (TFG.Flt --> TFG.Bol) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Flt --> (TFG.Flt --> TFG.Flt) <:>
        TFG.Flt --> (TFG.Flt --> TFG.Flt) <:>
        TFG.Flt --> (TFG.Flt --> TFG.Flt) <:>
        TFG.Flt --> (TFG.Flt --> TFG.Flt) <:>
        TFG.Cmx --> (TFG.Cmx --> TFG.Cmx) <:>
        TFG.Cmx --> (TFG.Cmx --> TFG.Cmx) <:>
        TFG.Cmx --> (TFG.Cmx --> TFG.Cmx) <:>
        TFG.Cmx --> (TFG.Cmx --> TFG.Cmx) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> TFG.Int               <:>
        TFG.Int --> TFG.Flt               <:>
        TFG.Flt --> TFG.Cmx               <:>
        TFG.Int --> TFG.Int               <:>
        TFG.Flt --> TFG.Flt               <:>
        TFG.Ary TFG.Flt --> TFG.Ary TFG.Flt <:>
        TFG.Ary TFG.Int                   <:>
        ET.Emp

---------------------------------------------------------------------------------
-- ESTH
---------------------------------------------------------------------------------

esTH :: ES.Env (Len Prelude) TH.Name
esTH = 'realPartHsk
   <+> 'imagPartHsk
   <+> 'eqlBolHsk
   <+> 'eqlIntHsk
   <+> 'eqlFltHsk
   <+> 'ltdBolHsk
   <+> 'ltdIntHsk
   <+> 'ltdFltHsk
   <+> 'addIntHsk
   <+> 'subIntHsk
   <+> 'mulIntHsk
   <+> 'divIntHsk
   <+> 'addFltHsk
   <+> 'subFltHsk
   <+> 'mulFltHsk
   <+> 'divFltHsk
   <+> 'addCmxHsk
   <+> 'subCmxHsk
   <+> 'mulCmxHsk
   <+> 'divCmxHsk
   <+> 'andIntHsk
   <+> 'orIntHsk
   <+> 'xorIntHsk
   <+> 'shrIntHsk
   <+> 'shlIntHsk
   <+> 'cmpIntHsk
   <+> 'i2fHsk
   <+> 'cisHsk
   <+> 'ilog2Hsk
   <+> 'sqrtFltHsk
   <+> 'memHsk
   <+> 'hshTblHsk
   <+> ES.Emp

---------------------------------------------------------------------------------
-- ESString
---------------------------------------------------------------------------------

esString :: ES.Env (Len Prelude) String
esString = fmap (\ (TH.Name x _) -> (init . init . init . TH.occString) x) esTH

---------------------------------------------------------------------------------
-- ESFAV
---------------------------------------------------------------------------------

esFAV :: ES.Env (Len Prelude) FAV.Exp
esFAV = realPartFAV
   <+> imagPartFAV
   <+> eqlBolFAV
   <+> eqlIntFAV
   <+> eqlFltFAV
   <+> ltdBolFAV
   <+> ltdIntFAV
   <+> ltdFltFAV
   <+> addIntFAV
   <+> subIntFAV
   <+> mulIntFAV
   <+> divIntFAV
   <+> addFltFAV
   <+> subFltFAV
   <+> mulFltFAV
   <+> divFltFAV
   <+> addCmxFAV
   <+> subCmxFAV
   <+> mulCmxFAV
   <+> divCmxFAV
   <+> andIntFAV
   <+> orIntFAV
   <+> xorIntFAV
   <+> shrIntFAV
   <+> shlIntFAV
   <+> cmpIntFAV
   <+> i2fFAV
   <+> cisFAV
   <+> ilog2FAV
   <+> sqrtFltFAV
   <+> memFAV
   <+> hshTblFAV
   <+> ES.Emp

---------------------------------------------------------------------------------
-- etFGV
---------------------------------------------------------------------------------

etFGV :: ET.Env FGV.Exp Prelude
etFGV = (FGV.Exp realPartHsk
     <:> FGV.Exp imagPartHsk
     <:> FGV.Exp eqlBolHsk
     <:> FGV.Exp eqlIntHsk
     <:> FGV.Exp eqlFltHsk
     <:> FGV.Exp ltdBolHsk
     <:> FGV.Exp ltdIntHsk
     <:> FGV.Exp ltdFltHsk
     <:> FGV.Exp addIntHsk
     <:> FGV.Exp subIntHsk
     <:> FGV.Exp mulIntHsk
     <:> FGV.Exp divIntHsk
     <:> FGV.Exp addFltHsk
     <:> FGV.Exp subFltHsk
     <:> FGV.Exp mulFltHsk
     <:> FGV.Exp divFltHsk
     <:> FGV.Exp addCmxHsk
     <:> FGV.Exp subCmxHsk
     <:> FGV.Exp mulCmxHsk
     <:> FGV.Exp divCmxHsk
     <:> FGV.Exp andIntHsk
     <:> FGV.Exp orIntHsk
     <:> FGV.Exp xorIntHsk
     <:> FGV.Exp shrIntHsk
     <:> FGV.Exp shlIntHsk
     <:> FGV.Exp cmpIntHsk
     <:> FGV.Exp i2fHsk
     <:> FGV.Exp cisHsk
     <:> FGV.Exp ilog2Hsk
     <:> FGV.Exp sqrtFltHsk
     <:> FGV.Exp memHsk
     <:> FGV.Exp hshTblHsk
     <:> ET.Emp)

---------------------------------------------------------------------------------
-- EMTHFAV
---------------------------------------------------------------------------------

emTHFAV :: EM.Env TH.Name FAV.Exp
emTHFAV = let is :: EP.Env TH.Name = frmRgt (cnv (esTH  , ()))
              es :: EP.Env FAV.Exp = frmRgt (cnv (esFAV , ()))
          in zip is es

---------------------------------------------------------------------------------
-- Var
---------------------------------------------------------------------------------

realPartVar :: Var Prelude (TFA.Cmx :-> TFA.Flt)
realPartVar = $(nat 0 "")

imagPartVar :: Var Prelude (TFA.Cmx :-> TFA.Flt)
imagPartVar = $(nat 1 "")

eqlBolVar :: Var Prelude (TFA.Bol :-> (TFA.Bol :-> TFA.Bol))
eqlBolVar = $(nat 2 "")

eqlIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Bol))
eqlIntVar = $(nat 3 "")

eqlFltVar :: Var Prelude (TFA.Flt :-> (TFA.Flt :-> TFA.Bol))
eqlFltVar = $(nat 4 "")

ltdBolVar :: Var Prelude (TFA.Bol :-> (TFA.Bol :-> TFA.Bol))
ltdBolVar = $(nat 5 "")

ltdIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Bol))
ltdIntVar = $(nat 6 "")

ltdFltVar :: Var Prelude (TFA.Flt :-> (TFA.Flt :-> TFA.Bol))
ltdFltVar = $(nat 7 "")

addIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
addIntVar = $(nat 8 "")

subIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
subIntVar = $(nat 9 "")

mulIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
mulIntVar = $(nat 10 "")

divIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
divIntVar = $(nat 11 "")

addFltVar :: Var Prelude (TFA.Flt :-> (TFA.Flt :-> TFA.Flt))
addFltVar = $(nat 12 "")

subFltVar :: Var Prelude (TFA.Flt :-> (TFA.Flt :-> TFA.Flt))
subFltVar = $(nat 13 "")

mulFltVar :: Var Prelude (TFA.Flt :-> (TFA.Flt :-> TFA.Flt))
mulFltVar = $(nat 14 "")

divFltVar :: Var Prelude (TFA.Flt :-> (TFA.Flt :-> TFA.Flt))
divFltVar = $(nat 15 "")

addCmxVar :: Var Prelude (TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx))
addCmxVar = $(nat 16 "")

subCmxVar :: Var Prelude (TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx))
subCmxVar = $(nat 17 "")

mulCmxVar :: Var Prelude (TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx))
mulCmxVar = $(nat 18 "")

divCmxVar :: Var Prelude (TFA.Cmx :-> (TFA.Cmx :-> TFA.Cmx))
divCmxVar = $(nat 19 "")

andIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
andIntVar = $(nat 20 "")

orIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
orIntVar = $(nat 21 "")

xorIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
xorIntVar = $(nat 22 "")

shrIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
shrIntVar = $(nat 23 "")

shlIntVar :: Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
shlIntVar = $(nat 24 "")

cmpIntVar :: Var Prelude (TFA.Int :-> TFA.Int)
cmpIntVar = $(nat 25 "")

i2fVar :: Var Prelude (TFA.Int :-> TFA.Flt)
i2fVar = $(nat 26 "")

cisVar :: Var Prelude (TFA.Flt :-> TFA.Cmx)
cisVar = $(nat 27 "")

ilog2Var :: Var Prelude (TFA.Int :-> TFA.Int)
ilog2Var = $(nat 28 "")

sqrtFltVar ::  Var Prelude (TFA.Flt :-> TFA.Flt)
sqrtFltVar = $(nat 29 "")

memVar ::  Var Prelude (TFA.Ary TFA.Flt :-> TFA.Ary TFA.Flt)
memVar = $(nat 30 "")

hshTblVar ::  Var Prelude (TFA.Ary TFA.Int)
hshTblVar = $(nat 31 "")

---------------------------------------------------------------------------------
-- TH.Name
---------------------------------------------------------------------------------

type Bol = Bool
type Int = Word32
type Flt = Float
type Cmx = Complex Float

realPartHsk :: Cmx -> Flt
realPartHsk = realPart

imagPartHsk :: Cmx -> Flt
imagPartHsk = imagPart

eqlBolHsk :: Bol -> Bol -> Bol
eqlBolHsk = (==)

eqlIntHsk :: Int -> (Int -> Bol)
eqlIntHsk = (==)

eqlFltHsk :: Flt -> (Flt -> Bol)
eqlFltHsk = (==)

ltdBolHsk :: Bol -> (Bol -> Bol)
ltdBolHsk = (<)

ltdIntHsk :: Int -> (Int -> Bol)
ltdIntHsk = (<)

ltdFltHsk :: Flt -> (Flt -> Bol)
ltdFltHsk = (<)

addIntHsk :: Int -> (Int -> Int)
addIntHsk = (+)

subIntHsk :: Int -> (Int -> Int)
subIntHsk = (-)

mulIntHsk :: Int -> (Int -> Int)
mulIntHsk = (*)

divIntHsk :: Int -> (Int -> Int)
divIntHsk =  div

addFltHsk :: Flt -> (Flt -> Flt)
addFltHsk = (+)

subFltHsk :: Flt -> (Flt -> Flt)
subFltHsk = (-)

mulFltHsk :: Flt -> (Flt -> Flt)
mulFltHsk = (*)

divFltHsk :: Flt -> (Flt -> Flt)
divFltHsk = (/)

addCmxHsk :: Cmx -> (Cmx -> Cmx)
addCmxHsk = (+)

subCmxHsk :: Cmx -> (Cmx -> Cmx)
subCmxHsk = (-)

mulCmxHsk :: Cmx -> (Cmx -> Cmx)
mulCmxHsk = (*)

divCmxHsk :: Cmx -> (Cmx -> Cmx)
divCmxHsk = (/)

andIntHsk :: Int -> (Int -> Int)
andIntHsk = (.&.)

orIntHsk :: Int -> (Int -> Int)
orIntHsk = (.|.)

xorIntHsk :: Int -> (Int -> Int)
xorIntHsk = xor

shrIntHsk :: Int -> (Int -> Int)
shrIntHsk i j = shiftR i (fromIntegral j)

shlIntHsk :: Int -> (Int -> Int)
shlIntHsk i j = shiftL i (fromIntegral j)

cmpIntHsk :: Int -> Int
cmpIntHsk = complement

i2fHsk :: Int -> Flt
i2fHsk = (\ i -> fromIntegral i)

cisHsk :: Flt -> Cmx
cisHsk = cis

ilog2Hsk :: Int -> Int
ilog2Hsk x = floor (log (fromIntegral x) / log (2 :: Float))

sqrtFltHsk :: Flt -> Flt
sqrtFltHsk = sqrt

memHsk :: Array Integer Flt -> Array Integer Flt
memHsk = id

hshTblHsk :: Array Integer Integer
hshTblHsk = listArray (0,255) [
  0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419,
  0x706af48f, 0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4,
  0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07,
  0x90bf1d91, 0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
  0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856,
  0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
  0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4,
  0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
  0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3,
  0x45df5c75, 0xdcd60dcf, 0xabd13d59, 0x26d930ac, 0x51de003a,
  0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599,
  0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
  0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190,
  0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f,
  0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0x0f00f934, 0x9609a88e,
  0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
  0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed,
  0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
  0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3,
  0xfbd44c65, 0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
  0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a,
  0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5,
  0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa, 0xbe0b1010,
  0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
  0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17,
  0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6,
  0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615,
  0x73dc1683, 0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
  0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1, 0xf00f9344,
  0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
  0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a,
  0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
  0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1,
  0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c,
  0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef,
  0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
  0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe,
  0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31,
  0x2cd99e8b, 0x5bdeae1d, 0x9b64c2b0, 0xec63f226, 0x756aa39c,
  0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
  0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b,
  0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
  0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1,
  0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
  0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45, 0xa00ae278,
  0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7,
  0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66,
  0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
  0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605,
  0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8,
  0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b,
  0x2d02ef8d ]


---------------------------------------------------------------------------------
-- FAV
---------------------------------------------------------------------------------

realPartFAV :: FAV.Exp
realPartFAV = FAV.lft realPartHsk

imagPartFAV :: FAV.Exp
imagPartFAV = FAV.lft imagPartHsk

eqlBolFAV :: FAV.Exp
eqlBolFAV = FAV.lft eqlBolHsk

eqlIntFAV :: FAV.Exp
eqlIntFAV = FAV.lft eqlIntHsk

eqlFltFAV :: FAV.Exp
eqlFltFAV = FAV.lft eqlFltHsk

ltdBolFAV :: FAV.Exp
ltdBolFAV = FAV.lft ltdBolHsk

ltdIntFAV :: FAV.Exp
ltdIntFAV = FAV.lft ltdIntHsk

ltdFltFAV :: FAV.Exp
ltdFltFAV = FAV.lft ltdFltHsk

addIntFAV :: FAV.Exp
addIntFAV = FAV.lft addIntHsk

subIntFAV :: FAV.Exp
subIntFAV = FAV.lft subIntHsk

mulIntFAV :: FAV.Exp
mulIntFAV = FAV.lft mulIntHsk

divIntFAV :: FAV.Exp
divIntFAV = FAV.lft divIntHsk

addFltFAV :: FAV.Exp
addFltFAV = FAV.lft addFltHsk

subFltFAV :: FAV.Exp
subFltFAV = FAV.lft subFltHsk

mulFltFAV :: FAV.Exp
mulFltFAV = FAV.lft mulFltHsk

divFltFAV :: FAV.Exp
divFltFAV = FAV.lft divFltHsk

addCmxFAV :: FAV.Exp
addCmxFAV = FAV.lft addCmxHsk

subCmxFAV :: FAV.Exp
subCmxFAV = FAV.lft subCmxHsk

mulCmxFAV :: FAV.Exp
mulCmxFAV = FAV.lft mulCmxHsk

divCmxFAV :: FAV.Exp
divCmxFAV = FAV.lft divCmxHsk

andIntFAV :: FAV.Exp
andIntFAV = FAV.lft andIntHsk

orIntFAV :: FAV.Exp
orIntFAV = FAV.lft orIntHsk

xorIntFAV :: FAV.Exp
xorIntFAV = FAV.lft xorIntHsk

shrIntFAV :: FAV.Exp
shrIntFAV = FAV.lft shrIntHsk

shlIntFAV :: FAV.Exp
shlIntFAV = FAV.lft shlIntHsk

cmpIntFAV :: FAV.Exp
cmpIntFAV = FAV.lft cmpIntHsk

i2fFAV :: FAV.Exp
i2fFAV = FAV.lft i2fHsk

cisFAV :: FAV.Exp
cisFAV = FAV.lft  cisHsk

ilog2FAV :: FAV.Exp
ilog2FAV = FAV.lft ilog2Hsk

sqrtFltFAV :: FAV.Exp
sqrtFltFAV = FAV.lft sqrtFltHsk

memFAV :: FAV.Exp
memFAV = FAV.lft memHsk

hshTblFAV :: FAV.Exp
hshTblFAV = FAV.lft hshTblHsk