module Weaken where
 
import Prelude ()
import MyPrelude

import Variable.Typed

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import Data.IORef
import System.IO.Unsafe
import Singleton

type family Tif (b :: Bool) (t :: k) (r :: k) :: k where
  Tif True  t t' = t
  Tif False t t' = t' 

type Chs t ta r = Tif t r (ta ': r)

data Opt :: * -> Bool -> * where
  Has :: a -> Opt a True
  Non :: Opt a False
        
type a :-> b = TFA.Arr a b 

data Exp :: [TFA.Typ] -> TFA.Typ -> * where 
  Var  :: Var r t  -> Exp r t  
  Abs  :: (Opt (Exp r ta) t -> Exp (Chs t ta r) tb) -> Exp r (ta :-> tb) 
  App  :: HasSin TFG.Typ ta => 
          Exp r (ta :-> tb) -> Exp r ta -> Exp r tb     
   
var :: (Var r t -> Var r' t) -> Var r t -> Exp r' t
var inc = Var . inc


f1 :: forall ta tb r r'. 
      (r ~ (ta :-> tb ': r'), HasSin TFG.Typ ta) => Exp r (ta :-> tb)
f1 = Abs (\ x -> 
   let (var , e) = (case x of 
                       Has e' -> ((\ v -> Var v) 
                                  :: forall t. 
                                     HasSin TFG.Typ t => Var r t -> Exp r t
                                 , e' :: Exp r ta)
                       Non   ->  ((\ v -> Var (Suc v)) 
                                  :: forall t. HasSin TFG.Typ t => 
                                     Var r t -> Exp (ta ': r) t
                                 , Var Zro :: Exp (ta ': r) ta)) 
  in App (var Zro) e)  
{-
deriving instance Show (Exp r t)

ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

instance Show (Exp r ta -> Exp r tb) where
  show f =  
    
    let i = unsafePerformIO (do j <- readIORef ref
                                modifyIORef ref (+1)
                                return j)
        v = "_x" ++ show i
    in ("(Fun "++ v ++ " (" ++ 
        show (f (Tmp v)) 
        ++ "))")
 
sucAll :: Exp r t' -> Exp (t ': r) t' 
sucAll = mapVar Suc prd
                           
prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc
               
mapVar :: (forall t'. Var r  t' -> Var r' t') -> 
          (forall t'. Var r' t' -> Var r  t') -> 
          Exp r t -> Exp r' t 
          -}

{-
mapVar :: forall r r' t. 
          (forall t'. Var r t' -> Exp r' t') -> Exp r t -> Exp r' t
mapVar f ee = case ee of
  Var v        -> f v
  Abs eb       -> Abs (\ x -> mapVar (\ v -> case v of 
                                         Zro -> x
                                         _   -> Var v) 
                      (mf (eb (Var Zro))))
  App ef ea    -> App (m ef)  (m ea)
 
  where
    m :: Exp r tt -> Exp r' tt
    m  = mapVar f
    
    mf :: Exp (ta ': r) tt -> Exp (ta ': r') tt
    mf = mf -- mapVar (inc f)
 

-}