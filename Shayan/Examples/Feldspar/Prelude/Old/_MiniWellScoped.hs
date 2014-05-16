module Examples.Feldspar.Prelude.MiniWellScoped where

import Prelude ()
import MyPrelude 

import Expression.Feldspar.MiniWellScoped

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.ADT             as TFA
import qualified Type.Feldspar.GADT            as TFG

import Conversion 
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import Environment.Typed
import Singleton
   
import Examples.Feldspar.Prelude.Environment

type Data t = Exp Prelude t
type Vec  t = (Data TFA.Int , Data TFA.Int -> t)

(===) :: Data TFA.Int -> Data TFA.Int -> Data TFA.Bol
e1 === e2 = AppV eqlVar (Ext e1 (Ext e2 Emp)) 

(/==) :: Data TFA.Int -> Data TFA.Int -> Data TFA.Bol
e1 /== e2 =  AppV notVar (Ext (e1 === e2) Emp) 

(+.) :: Data TFA.Int -> Data TFA.Int -> Data TFA.Int
e1 +. e2 = AppV addVar (Ext e1 (Ext e2 Emp))  

(*.) :: Data TFA.Int -> Data TFA.Int -> Data TFA.Int
e1 *. e2 =  AppV mulVar (Ext e1 (Ext e2 Emp))  

minF :: Data TFA.Int -> Data TFA.Int -> Data TFA.Int
minF e1 e2 = AppV minVar (Ext e1 (Ext e2 Emp))  

(<.) :: Data TFA.Int -> Data TFA.Int -> Data TFA.Bol
e1 <. e2 =  AppV ltdVar (Ext e1 (Ext e2 Emp))  

div :: Data TFA.Int -> Data TFA.Int -> Data TFA.Int
div e1 e2 =  AppV divVar (Ext e1 (Ext e2 Emp))  

sumv :: Vec (Data TFA.Int) -> Data TFA.Int
sumv (l , ixf) = Fst (
               Whl ((/== l) . Snd) 
               (\ s -> Tpl (ixf (Snd s) +. (Fst s)) (Snd s +. ConI 1)) 
               (Tpl (ConI 0) (ConI 0)))

mapv :: (a -> b) -> Vec a -> Vec b
mapv f (l , ixf) = (l , f . ixf)

zipWithv :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWithv f (l1 , ixf1) (l2 , ixf2) = (minF l1 l2 , ixf)
  where ixf i = f (ixf1 i) (ixf2 i)

scalarProd :: Vec (Data TFA.Int) -> Vec (Data TFA.Int) -> 
              Data TFA.Int
scalarProd vecA vecB = sumv (zipWithv (*.) vecA vecB)

axpy :: Data TFA.Int -> Vec (Data TFA.Int) -> 
        Vec (Data TFA.Int) -> Vec (Data TFA.Int)
axpy a x y = zipWithv (+.) (mapv (a*.) x) y

tst :: Data TFA.Int 
tst = scalarProd  (ConI 2 , id) (ConI 2 , ((+.) (ConI 1)))

test :: Bool
test = case curry cnv tst etFGV of 
  Rgt ((FGV.Exp x) :: FGV.Exp TFA.Int) -> x == 2
  _                                -> False
   
forLoop :: HasSin TFG.Typ s => Data TFA.Int -> Data s -> 
           (Data TFA.Int -> Data s -> Data s ) -> Data s
forLoop l init step = Snd (Whl (\ t -> (Fst t) <. l)
                               (\ t -> Tpl ((Fst t) +. (ConI 1)) 
                                                    (step (Fst t) (Snd t)))
                               (Tpl (ConI 0) init))
                      
fac :: Data TFA.Int -> Data TFA.Int
fac n = forLoop (n +. (ConI 1)) (ConI 1) (*.) 

{-
cnvVec :: HasSin TFG.Typ t => [FGV.Exp t] -> Vec (Data t)
cnvVec vs = ( ConI (toInteger (length vs)) ,  
              \ ei -> case frmRgt (cnv (ei , env)) :: FGV.Exp TFA.Int of
                FGV.Exp i -> frmRgt (cnv  (vs !! (fromInteger i), env)))

cnvLst :: HasSin TFG.Typ t => Vec (Data t) -> [FGV.Exp t] 
cnvLst (l , ixf) = let FGV.Exp i = frmRgt (cnv (l , env)) :: FGV.Exp TFA.Int
                   in  frmRgt (P.mapM (flip (curry cnv) env . ixf . ConI) 
                               [0..(i-1)])
-}   