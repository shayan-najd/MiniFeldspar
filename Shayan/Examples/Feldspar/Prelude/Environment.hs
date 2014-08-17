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
import Conversion.Environment ()

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
   <+> ES.Emp

---------------------------------------------------------------------------------
-- ESString
---------------------------------------------------------------------------------

esString :: ES.Env (Len Prelude) String
esString = ES.map (\ (TH.Name x _) -> (init . init . init . TH.occString) x) esTH
  
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
