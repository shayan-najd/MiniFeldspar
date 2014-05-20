

import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.FFT.Common

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

fft :: Data (Ary Complex -> Ary Complex)
fft = [|| \ v -> (\ steps -> $$bitRev steps ($$fftCore steps v)) 
                 ($$((-)) ($$ilog2 (len v))  1) ||]
         
fftCore :: Data (Integer -> Ary Complex -> Ary Complex)
fftCore = [|| \ n -> \ vv -> $$forLoop ($$((+)) n 1) vv 
                             (\ j -> \ v -> ary (len vv) 
                                            (\ i -> $$ixf v ($$((-)) n j) i)) ||]

ixf :: Data (Ary Complex -> Integer -> Integer -> Complex)
ixf = let p :: MP.Float = MP.negate MP.pi in
      [|| \ v -> \ k -> \ i ->
            (\ k2 -> (\ twid -> \ a -> \ b -> 
                       (if $$testBit i k 
                        then $$((*)) twid ($$((-)) b a) 
                        else $$((+)) a b)) 
                     ($$cis ($$((/)) ($$((*)) p ($$i2f ($$lsbs k i))) 
                        ($$i2f k2))) 
                     (ind v i)  
                     (ind v ($$xor i k2)))
            ($$((.<<.)) 1 k)
      ||]
           
bitRev :: Data (Integer -> Ary Complex -> Ary Complex)
bitRev = [|| \ n -> \ x -> 
             $$forLoop n x (\ i -> $$permute (\ _j -> $$rotBit ($$((+)) i 1)))
         ||]

rotBit :: Data (Integer -> Integer -> Integer)
rotBit = [|| \ k -> \ i -> $$((.|.)) 
                           ($$((.<<.)) 
                            ($$((.|.))
                             ($$((.<<.)) 
                              ($$((.>>.))                 
                               ($$((.>>.)) i 1) k) 1) 
                             ($$((.&.)) i 1)) k) 
                           ($$lsbs k ($$((.>>.)) i 1 )) ||]
 
inp :: Data (Ary Complex)
inp = fromList (MP.fmap (\ f -> [|| cmx f 0.0 ||]) tstInp) 
      [|| cmx 0.0 0.0 ||]
 
out :: [MP.Float]
out  = let outFMWS :: FMWS.Exp Prelude (TFA.Ary TFA.Cmx) =
             MP.frmRgt (cnv ([|| $$fft $$inp ||] , etTFG , esTH))
           (FGV.Exp e) :: FGV.Exp (TFA.Ary TFA.Cmx) = 
             MP.frmRgt (cnv (outFMWS , etFGV)) 
       in MP.fmap MP.magnitude (MP.elems e) 

prop :: MP.Bool
prop = test out

dummyAry :: Ary Complex
dummyAry = dummyAry
 
fftFMWS :: FMWS.Exp (TFA.Ary TFA.Cmx ': Prelude) (TFA.Ary TFA.Cmx)
fftFMWS = MP.frmRgt (cnv ([|| $$fft dummyAry ||] 
                         , TFG.Ary TFG.Cmx <:> etTFG 
                         , 'dummyAry <+> esTH))

main :: MP.IO ()
main = MP.getArgs MP.>>=  
       (\ [as] -> let f = MP.frmRgt 
                          (scompileWith [("v0" , TFA.Ary TFA.Cmx)]  
                           (TFG.Ary TFG.Cmx) 
                           ("v0" <+> esString) 1 
                           (nrmIf (as MP./= "NoNrm") fftFMWS)) 
                  in MP.writeFile (as MP.++ "FFTTemplateHaskell.c") f)