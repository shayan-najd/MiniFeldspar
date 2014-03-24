{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Examples.Feldspar.Prelude.TemplateHaskell where

import Prelude ()
import MyPrelude hiding (fst,snd)
import qualified Expression.Feldspar.ADTValue        as FAV
import VanillaPrelude 

import Language.Haskell.TH.Syntax
import qualified Expression.Feldspar.ADTUntypedNamed as FAUN
import qualified Type.Feldspar.ADT                   as TFA
import qualified Type.Feldspar.GADT                  as TFG

import Conversion 
import Conversion.Expression.Feldspar ()
import Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed ()

import qualified Environment.Map    as EM
import qualified Environment.Scoped as ES
import qualified Environment.Typed  as ET
 
import qualified Nat.ADT            as NA

type ta :-> tb = TFA.Arr ta tb

(-->) :: TFG.Typ a -> TFG.Typ b -> TFG.Typ (a :-> b)
(-->) = TFG.Arr
  
infixr 5 <:>
(<:>) :: tf t -> ET.Env tf e -> ET.Env tf (t ': e)
(<:>) = ET.Ext

infixr 5 <->
(<->) :: t -> ES.Env n t -> ES.Env (NA.Suc n) t
(<->) = ES.Ext

type TypLst =  TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
               TFA.Bol :-> TFA.Bol ': 
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ': 
               TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
               TFA.Int :-> (TFA.Int :-> TFA.Int) ':
               '[]   
           
envVal :: EM.Env Name FAV.Exp
envVal = ('(==) , FAV.Abs (\ (FAV.ConI i) -> 
                    FAV.Abs (\ (FAV.ConI j) -> FAV.ConB (i == j))))  : 
         ('not  , FAV.Abs (\ (FAV.ConB b) -> FAV.ConB (not b)))      :
         ('(+)  , FAV.Abs (\ (FAV.ConI i) -> 
                    FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (i + j))))   :
         ('(*)  , FAV.Abs (\ (FAV.ConI i) -> 
                    FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (i * j))))   :
         ('min  , FAV.Abs (\ (FAV.ConI i) -> 
                    FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (min i j)))) :
         ('(<)  , FAV.Abs (\ (FAV.ConI i) -> 
                    FAV.Abs (\ (FAV.ConI j) -> FAV.ConB (i < j))))   :
         ('div  , FAV.Abs (\ (FAV.ConI i) -> 
                    FAV.Abs (\ (FAV.ConI j) -> FAV.ConI (div i j)))) :
         []

envNam :: ES.Env (NA.Suc (NA.Suc (NA.Suc (NA.Suc 
                  (NA.Suc (NA.Suc (NA.Suc NA.Zro))))))) Name
envNam = '(==) <-> 
         'not  <->
         '(+)  <->                 
         '(*)  <->                  
         'min  <->                           
         '(<)  <->
         'div  <->
         ES.Emp
 
envTyp :: ET.Env TFG.Typ TypLst          
envTyp = TFG.Int --> (TFG.Int --> TFG.Bol) <:>
         TFG.Bol --> TFG.Bol               <:> 
         TFG.Int --> (TFG.Int --> TFG.Int) <:>
         TFG.Int --> (TFG.Int --> TFG.Int) <:>
         TFG.Int --> (TFG.Int --> TFG.Int) <:> 
         TFG.Int --> (TFG.Int --> TFG.Bol) <:> 
         TFG.Int --> (TFG.Int --> TFG.Int) <:> 
         ET.Emp

type Data t = Q (TExp t)
type Vec  t = (Data (Integer -> t) , Data Integer)
 
(/==) :: Data (Integer -> Integer -> Bool)
(/==) = [|| \ e1 -> \ e2 -> not (e1 == e2) ||] 

sumv :: Vec Integer -> Data Integer
sumv (ixf , l) = [|| fst (whl 
                          (\ x -> ( $$((/==)) (snd x) $$l)) 
                          (\ s -> (($$ixf (snd s)) + (fst s) , (snd s) + 1 )) 
                          (0 , 0)) 
                  ||]
  
mapv :: Data (a -> b) -> Vec a -> Vec b
mapv f (ixf,l) = ([|| \ x -> $$f ($$ixf x) ||] , l)

zipWithv :: Data (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf , [|| min $$l1 $$l2 ||])
  where ixf = [|| \ i -> $$f ($$ixf1 i) ($$ixf2 i) ||]
 

scalarProd :: Vec Integer -> Vec Integer -> Data Integer
scalarProd vecA vecB = sumv (zipWithv [|| (*) ||] vecA vecB)

axpy :: Data Integer -> Vec Integer -> Vec Integer -> Vec Integer
axpy a x y = zipWithv [|| (+) ||] (mapv [|| \ b ->  $$a * b ||] x) y

tst :: Data Integer
tst = scalarProd ([||\ x -> x ||] , [|| 2 ||]) ([||\ x -> x + 1 ||] , [|| 2 ||])

test :: Bool
test = case (do tst' :: FAUN.Exp Name <- cnv (tst , envTyp , envNam) 
                cnv (tst' , envVal)) of
         Rgt (FAV.ConI x) -> x == 2
         _                -> False
                  
cnvVec :: [Integer] -> Vec Integer
cnvVec vss = ( g vss 
            , [|| l ||] )
  where
    l  = (toInteger . length) vss
    g  = f 0 
    f _ []       = [|| \ x -> 0 ||]
    f i (v : vs) = [|| \ x -> if (x == i) 
                              then v
                              else $$(f (i+1) vs) x 
                   ||]

cnvLst :: Vec Integer -> [Integer] 
cnvLst (ixf , l) = frmRgt (
                   do ixf' :: FAUN.Exp Name <- cnv (ixf  , envTyp , envNam) 
                      FAV.Abs f             <- cnv (ixf' , envVal)
                      l'   :: FAUN.Exp Name <- cnv (l    , envTyp , envNam)
                      FAV.ConI i            <- cnv (l'   , envVal)
                      return (fmap ((\ (FAV.ConI k) -> k) . f . FAV.ConI) 
                              [0..(i-1)]))