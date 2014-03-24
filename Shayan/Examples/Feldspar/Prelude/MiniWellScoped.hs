module Examples.Feldspar.Prelude.MiniWellScoped where

import Prelude ()
import MyPrelude hiding (div)
import qualified MyPrelude as P 

import Expression.Feldspar.MiniWellScoped
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.ADT             as TFA
import qualified Type.Feldspar.GADT            as TFG

import Conversion 
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import Environment.Typed
import Variable.Typed
import Singleton

type ta :-> tb = TFA.Arr ta tb

infixr 5 <:>
(<:>) :: tf t -> Env tf e -> Env tf (t ': e)
(<:>) = Ext

type TypLst = TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
              TFA.Bol :-> TFA.Bol ': 
              TFA.Int :-> (TFA.Int :-> TFA.Int) ':
              TFA.Int :-> (TFA.Int :-> TFA.Int) ':
              TFA.Int :-> (TFA.Int :-> TFA.Int) ': 
              TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
              TFA.Int :-> (TFA.Int :-> TFA.Int) ':
              '[]   
           
env :: Env FGV.Exp TypLst
env = (FGV.Exp (==))    <:>
      (FGV.Exp not)     <:>
      (FGV.Exp (+))     <:>                
      (FGV.Exp (*))     <:>                
      (FGV.Exp min)     <:>                           
      (FGV.Exp (<))     <:>
      (FGV.Exp (P.div)) <:> 
      Emp
        
type Data t = Exp TypLst t
type Vec  t = (Data TFA.Int -> t , Data TFA.Int)

eqlVar :: Var TypLst (TFA.Int :-> (TFA.Int :-> TFA.Bol))
eqlVar = Zro

notVar :: Var TypLst (TFA.Bol :-> TFA.Bol)
notVar = Suc Zro

addVar :: Var TypLst (TFA.Int :-> (TFA.Int :-> TFA.Int))
addVar = Suc (Suc Zro)

mulVar :: Var TypLst (TFA.Int :-> (TFA.Int :-> TFA.Int))
mulVar = Suc (Suc (Suc Zro))
  
minVar :: Var TypLst (TFA.Int :-> (TFA.Int :-> TFA.Int))
minVar = Suc (Suc (Suc (Suc Zro)))

ltdVar :: Var TypLst (TFA.Int :-> (TFA.Int :-> TFA.Bol))
ltdVar = Suc (Suc (Suc (Suc (Suc Zro))))

divVar :: Var TypLst (TFA.Int :-> (TFA.Int :-> TFA.Int))
divVar = Suc (Suc (Suc (Suc (Suc (Suc Zro)))))

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
sumv (ixf,l) = Fst (
               Whl ((/== l) . Snd) 
               (\ s -> Tpl (ixf (Snd s) +. (Fst s)) (Snd s +. ConI 1)) 
               (Tpl (ConI 0) (ConI 0)))

mapv :: (a -> b) -> Vec a -> Vec b
mapv f (ixf,l) = (f. ixf,l)

zipWithv :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf,minF l1 l2)
  where ixf i = f (ixf1 i) (ixf2 i)

scalarProd :: Vec (Data TFA.Int) -> Vec (Data TFA.Int) -> 
              Data TFA.Int
scalarProd vecA vecB = sumv (zipWithv (*.) vecA vecB)

axpy :: Data TFA.Int -> Vec (Data TFA.Int) -> 
        Vec (Data TFA.Int) -> Vec (Data TFA.Int)
axpy a x y = zipWithv (+.) (mapv (a*.) x) y

tst :: Data TFA.Int 
tst = (scalarProd  (id,ConI 2) (((+.) (ConI 1)),ConI 2))

test :: Bool
test = case curry cnv tst env of 
  Rgt ((FGV.Exp x) :: FGV.Exp TFA.Int) -> x == 2
  _                                -> False
   
forLoop :: Data TFA.Int -> Data s -> 
           (Data TFA.Int -> Data s -> Data s ) -> Data s
forLoop l init step = Snd (Whl (\ (Tpl i  _) -> i <. l)
                               (\ (Tpl i  s) -> Tpl (i +. (ConI 1)) 
                                                    (step i s))
                               (Tpl (ConI 0) init))
                      
cnvVec :: HasSin TFG.Typ t => [FGV.Exp t] -> Vec (Data t)
cnvVec vs = ( \ ei -> case frmRgt (cnv (ei , env)) :: FGV.Exp TFA.Int of
                 FGV.Exp i -> frmRgt (cnv  (vs !! (fromInteger i), env)) 
            , ConI (toInteger (length vs)))

cnvLst :: HasSin TFG.Typ t => Vec (Data t) -> [FGV.Exp t] 
cnvLst (ixf , l) = let FGV.Exp i = frmRgt (cnv (l , env)) :: FGV.Exp TFA.Int
                   in  frmRgt (P.mapM (flip (curry cnv) env . ixf . ConI) 
                               [0..(i-1)])
   