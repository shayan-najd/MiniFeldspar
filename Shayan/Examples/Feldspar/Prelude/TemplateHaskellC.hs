module Examples.Feldspar.Prelude.TemplateHaskellC where

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
import qualified Variable.Scoped                     as VS 
import Nat.TH  

dummyVec1 :: Ary Integer
dummyVec1 = dummyVec1

dummyVec2 :: Ary Integer
dummyVec2 = dummyVec2

es :: ES.Env (NA.Suc (NA.Suc (Len Prelude))) TH.Name
es = 'dummyVec2 <+> ('dummyVec1 <+> esTH)

et :: ET.Env TFG.Typ (TFA.Ary TFA.Int ': TFA.Ary TFA.Int ': Prelude)
et = TFG.Ary TFG.Int <:> (TFG.Ary TFG.Int <:> etTFG)
 
scalarProdFGHO :: FGHO.Exp 
                  (TFA.Ary TFA.Int ': TFA.Ary TFA.Int ': Prelude) TFA.Int
scalarProdFGHO = nrm (MP.frmRgt 
                      (cnv ([|| $$scalarProd dummyVec1 dummyVec2 ||] , et , es)))

scalarProdFMWS :: FMWS.Exp (TFA.Ary TFA.Int ': TFA.Ary TFA.Int ': Prelude) 
                  TFA.Int
scalarProdFMWS = MP.frmRgt (cnv (scalarProdFGHO , et , es))

scalarProdC = MP.frmRgt 
              (scompileWith 
               [("v0" , TFA.Ary TFA.Int), ("v1" , TFA.Ary TFA.Int)]  
               TFG.Int ("v0" <+> ("v1" <+> esString)) 2 scalarProdFMWS) 
 