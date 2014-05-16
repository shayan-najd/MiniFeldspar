module Examples.Feldspar.Prelude.Environment
       (Prelude,emTHFAV,esTH,esString,etFGV,etTFG,
        eqlVar,notVar,addVar,mulVar,minVar,ltdVar,divVar,(<+>),(<:>),toData) 
       where

import Prelude ()
import MyPrelude (String)
import qualified MyPrelude as MP 

import qualified Nat.ADT  as NA

import qualified Expression.Feldspar.ADTValue  as FAV
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Language.Haskell.TH.Syntax    as TH

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import qualified Environment.Map    as EM
import qualified Environment.Scoped as ES
import qualified Environment.Typed  as ET

import qualified Variable.Typed as VT
import Singleton

type ta :-> tb = TFA.Arr ta tb

(-->) :: TFG.Typ a -> TFG.Typ b -> TFG.Typ (a :-> b)
(-->) = TFG.Arr

infixr 5 <:>
(<:>) :: tf t -> ET.Env tf e -> ET.Env tf (t ': e)
(<:>) = ET.Ext

infixr 5 <+>
(<+>) :: t -> ES.Env n t -> ES.Env (NA.Suc n) t
(<+>) = ES.Ext

type Prelude = TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
              TFA.Bol :-> TFA.Bol               ': 
              TFA.Int :-> (TFA.Int :-> TFA.Int) ':
              TFA.Int :-> (TFA.Int :-> TFA.Int) ':
              TFA.Int :-> (TFA.Int :-> TFA.Int) ': 
              TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
              TFA.Int :-> (TFA.Int :-> TFA.Int) ':
              '[]

emTHFAV :: EM.Env TH.Name FAV.Exp
emTHFAV = ('(MP.==) , FAV.Abs (\ (FAV.ConI i) -> 
                        FAV.Abs (\ (FAV.ConI j) -> FAV.ConB (i MP.== j))))  : 
          ('MP.not  , FAV.Abs (\ (FAV.ConB b) -> FAV.ConB (MP.not b)))      :
          ('(MP.+)  , FAV.Abs (\ (FAV.ConI i) -> 
                        FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (i MP.+ j))))   :
          ('(MP.*)  , FAV.Abs (\ (FAV.ConI i) -> 
                        FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (i MP.* j))))   :
          ('MP.min  , FAV.Abs (\ (FAV.ConI i) -> 
                        FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (MP.min i j)))) :
          ('(MP.<)  , FAV.Abs (\ (FAV.ConI i) -> 
                        FAV.Abs (\ (FAV.ConI j) -> FAV.ConB (i MP.< j))))   :
          ('MP.div  , FAV.Abs (\ (FAV.ConI i) -> 
                        FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (MP.div i j)))) :
          []
                      
esTH :: ES.Env (Len Prelude) TH.Name
esTH = '(MP.==) <+> 
       'MP.not  <+>
       '(MP.+)  <+>                 
       '(MP.*)  <+>                  
       'MP.min  <+>                           
       '(MP.<)  <+>
       'MP.div  <+>
       ES.Emp
 
esString :: ES.Env (Len Prelude) String
esString = "eqlInt" <+>
           "not"    <+>  
           "add"    <+> 
           "mul"    <+>
           "min"    <+>
           "ltd"    <+>
           "div"    <+>
           ES.Emp

etFGV :: ET.Env FGV.Exp Prelude
etFGV = (FGV.Exp (MP.==)) <:>
        (FGV.Exp MP.not)  <:>
        (FGV.Exp (MP.+))  <:>                
        (FGV.Exp (MP.*))  <:>                
        (FGV.Exp MP.min)  <:>                           
        (FGV.Exp (MP.<))  <:>
        (FGV.Exp MP.div)  <:> 
        ET.Emp

etTFG :: ET.Env TFG.Typ Prelude          
etTFG = TFG.Int --> (TFG.Int --> TFG.Bol) <:>
        TFG.Bol --> TFG.Bol               <:> 
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:>
        TFG.Int --> (TFG.Int --> TFG.Int) <:> 
        TFG.Int --> (TFG.Int --> TFG.Bol) <:> 
        TFG.Int --> (TFG.Int --> TFG.Int) <:> 
        ET.Emp

eqlVar :: VT.Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Bol))
eqlVar = VT.Zro

notVar :: VT.Var Prelude (TFA.Bol :-> TFA.Bol)
notVar = VT.Suc VT.Zro

addVar :: VT.Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
addVar = VT.Suc (VT.Suc VT.Zro)

mulVar :: VT.Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
mulVar = VT.Suc (VT.Suc (VT.Suc VT.Zro))
  
minVar :: VT.Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
minVar = VT.Suc (VT.Suc (VT.Suc (VT.Suc VT.Zro)))

ltdVar :: VT.Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Bol))
ltdVar = VT.Suc (VT.Suc (VT.Suc (VT.Suc (VT.Suc VT.Zro))))

divVar :: VT.Var Prelude (TFA.Int :-> (TFA.Int :-> TFA.Int))
divVar = VT.Suc (VT.Suc (VT.Suc (VT.Suc (VT.Suc (VT.Suc VT.Zro)))))

toData :: TH.Name -> TH.Q (TH.TExp a)   
toData n = TH.unsafeTExpCoerce (TH.returnQ (TH.VarE n))
