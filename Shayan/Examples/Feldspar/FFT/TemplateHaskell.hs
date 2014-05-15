

import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment

import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()
 
import qualified Type.Feldspar.GADT                  as TFG
import Compiler (scompileWith)

import qualified Language.Haskell.TH.Syntax          as TH
import qualified Expression.Feldspar.MiniWellScoped  as FMWS
import qualified Type.Feldspar.ADT                   as TFA
import Conversion.Expression.Feldspar ()

import qualified Environment.Scoped                  as ES
import qualified Environment.Typed                   as ET
import Singleton (Len)
import qualified Nat.ADT                             as NA 

fft :: Data (Ary Complex -> Ary Complex)
fft = [|| \ v ->  $$forLoop (len v) v
                  (\ _i -> 
                    \ vv -> ary (len v) (\ i -> ind vv i)) ||]
        
dummyAry :: Ary Complex
dummyAry = dummyAry

es :: ES.Env (NA.Suc (Len Prelude)) TH.Name
es = 'dummyAry <+> esTH      

et :: ET.Env TFG.Typ (TFA.Ary TFA.Cmx ': Prelude)
et = TFG.Ary TFG.Cmx <:> etTFG
 
fftFMWS :: FMWS.Exp (TFA.Ary TFA.Cmx ': Prelude) (TFA.Ary TFA.Cmx)
fftFMWS = MP.frmRgt (cnv ([|| $$fft dummyAry ||] , et , es))

main :: MP.IO ()
main = let f = MP.frmRgt (scompileWith [("v0" , TFA.Ary TFA.Cmx)]  
                          (TFG.Ary TFG.Cmx) ("v0" <+> esString) 1 fftFMWS) 
       in MP.putStrLn f