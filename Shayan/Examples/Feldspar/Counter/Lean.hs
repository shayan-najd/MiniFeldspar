import Prelude ()
import MyPrelude (Integer,Bool(..))
import Type.Feldspar.ADT  
import Type.Feldspar.GADT (Arg,Out)
import Variable.Typed
import Environment.Typed     
import Singleton hiding (Trm)

type a :-> b = Arr a b

data Trm :: [Typ] -> Typ -> * where 
  ConI :: Integer  -> Trm r Int   
  Var    :: Var r t  -> Trm r t  
  Abs    :: Trm (ta ': r) tb  -> Trm r (ta :-> tb) 
  App    :: Trm r (ta :-> tb) -> Trm r ta -> Trm r tb     
  Quartz :: Trm r t -> Trm r t       
  Pair   :: Trm r tf -> Trm r ts -> Trm r (Tpl tf ts) 
  Fst    :: Trm r (Tpl tf ts) -> Trm r tf 
  Snd    :: Trm r (Tpl tf ts) -> Trm r ts 
  -- ...
                                       
-- termination of normalization                                   
-- being left with Exp (ta -> tb)                                   
                                       
data TrmImp :: [Typ] -> Typ -> * where
  ConIImp  :: Integer  -> TrmImp r Int 
  AppV     :: Var r t  -> Env (TrmImp r) (Arg t) -> TrmImp r (Out t)
  Quartz   :: TrmImp r t  -> TrmImp r t            
  PairImp  :: TrmImp r tf -> TrmImp r ts -> TrmImp r (Tpl tf ts)
  FstImp   :: TrmImp r (Tpl tf ts) -> TrmImp r tf
  SndImp   :: TrmImp r (Tpl tf ts) -> TrmImp r ts
  -- ...          
               
type Vec t = Tpl Int (Int :-> t)

vec :: Trm r Int -> Trm r (Int :-> t) -> Trm r (Vec t)
vec l ixf = Pair l ixf

id :: Trm r (t :-> t) 
id = Abs (Var Zro)

-- tst :: Trm '[] Int
tst = Fst 
      (Snd 
       (Whl 
        (Fst (Var Zro)) 
        (Pair (ConB False) (Snd (Var Zro))) 
        (Pair (ConB True ) (vec (ConI 1) id)))) 
      


{-                                                      
tst_bad= FstImp (SndImp 
                 (WhlImp 
                  (\ _x0 -> FstImp _x0) 
                  (\ _x1 -> PairImp (ConBImp False) (SndImp _x1)) 
                  (PairImp (ConBImp True) 
                   ((PairImp (ConIImp 1) (????? (\ _x2 -> (_x2 :: TrmImp '[] Int)))))))) 
         -}
      
 