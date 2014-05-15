module Examples.Feldspar.Counter.TemplateHaskell where

import Prelude ()
import qualified MyPrelude as MP
import Examples.Feldspar.Prelude.TemplateHaskell hiding (forLoop)
import Examples.Feldspar.Prelude.Environment
import Conversion
import Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed ()
import Conversion.Expression.Feldspar ()
import qualified Language.Haskell.TH.Syntax          as TH
import qualified Expression.Feldspar.ADTUntypedNamed as FAUN
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUD
import qualified Expression.Feldspar.GADTTyped          as FGTD

import Expression.Feldspar.GADTHigherOrder as FGHO
import qualified Expression.Feldspar.MiniWellScoped  as FMWS
import qualified Expression.Feldspar.ADTValue        as FAV
import qualified Type.Feldspar.ADT                   as TFA
import qualified Type.Feldspar.GADT                  as TFG
import qualified Environment.Scoped                  as ES
import qualified Environment.Typed                   as ET
import qualified Data.Complex                        as CMPX
import qualified Nat.ADT                             as NA
import Normalization
import Normalization.Feldspar.GADTHigherOrder ()
import Compiler (scompileWith)
import Singleton 
import Nat.TH   as NT
import Variable.Typed 

prop :: MP.Bool
prop = out MP.== 
       lst 

lst :: [Float]
lst = [4.71238898038469,
       3.141592653589793,
       1.5707963267948966,
       0.0,
       1.5707963267948966,
       3.141592653589793,
       4.71238898038469,
       6.283185307179586]

inp :: Data (Vec Float)
inp = fromList (MP.fmap (\ f -> [|| f ||]) lst) 
      [|| 0.0 ||]

res :: Data (Ary Float)
res = [|| $$vec2ary ($$id $$inp) ||]

out :: [MP.Float]
out = let outFAUN :: FAUN.Exp TH.Name = MP.frmRgt (cnv (res , etTFG , esTH ))
          e :: FAV.Exp   = MP.frmRgt (cnv (outFAUN , emTHFAV))
       in MP.elems (FAV.colft e :: MP.Array Integer Float)

dummyVec :: Ary Float
dummyVec = dummyVec

es :: ES.Env (NA.Suc (Len Prelude)) TH.Name
es = 'dummyVec <+> esTH      

et :: ET.Env TFG.Typ (TFA.Ary TFA.Flt ': Prelude)
et = TFG.Ary TFG.Flt <:> etTFG
--------------------------------------------------------------------

fftFGHO :: FGHO.Exp (TFA.Ary TFA.Flt ': Prelude) (TFA.Ary TFA.Flt)
fftFGHO = nrm (MP.frmRgt (cnv ([|| $$fft' dummyVec ||] , et , es)))

fftFMWS :: FMWS.Exp (TFA.Ary TFA.Flt ': Prelude) (TFA.Ary TFA.Flt)
fftFMWS = MP.frmRgt (cnv (fftFGHO , et , es)) 
 
fftC :: MP.IO ()
fftC = let f = MP.frmRgt (scompileWith [("v0" , TFA.Ary TFA.Flt)]  
                          (TFG.Ary TFG.Flt) ("v0" <+> esString) 1 fftFMWS) 
           in MP.writeFile "/home/tester/TH.c" f 


fft' :: Data (Ary Float -> Ary Float)
fft' = [|| \ a -> $$vec2ary ($$id ($$ary2vec a)) ||]

    
forLoop :: Data (s -> s)
forLoop = let zro :: Integer = 0 in
  [|| 
  \ init -> snd (whl 
  (\ t -> $$((<)) (fst t) 1)
  (\ t -> $$tpl ($$((+)) (fst t) 1) (snd t))
  (zro , init)) ||]
 
id :: Data (Vec Float -> Vec Float)
id = [|| \ vi -> $$forLoop vi  ||]
  
-- tst :: FGHO.Exp (TFA.Ary TFA.Flt ': Prelude) (TFA.Ary TFA.Int)

tst :: Exp Prelude TFA.Int
tst = Fst (Snd 
           (Whl 
            (\ _x0 -> Fst _x0) 
            (\ _x1 -> (Tpl (ConB False) (Snd _x1))) 
            (Tpl (ConB True) 
                 (Tpl (ConI 1) (Abs (\ _x2 -> App (App (Var addIntVar) _x2) 
                                              (ConI 1))))))) 

idd :: Data (Integer -> Integer)
idd = [|| \ i -> $$((+)) i 1 ||]

tst' :: Data Integer
tst' = [|| fst (snd (whl (\ s -> fst s) 
                     (\ s -> (False , snd s))
                     (True , ( 1 , $$idd)))) ||]
       
tstFGHO :: 
           FGHO.Exp Prelude TFA.Int 
tstFGHO = MP.frmRgt (cnv (tst' , etTFG , esTH))