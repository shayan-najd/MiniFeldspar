module Expression.Feldspar.MiniWellScoped where
 
import MyPrelude

import qualified Type.Feldspar.ADT      as TFA
import qualified Type.Feldspar.GADT     as TFG

import Variable.Typed

import Environment.Typed as ET     
import Data.IORef
import System.IO.Unsafe

import Singleton  

data Exp :: [TFA.Typ] -> TFA.Typ -> * where
  ConI  :: Integer  -> Exp r TFA.Int
  ConB  :: Bool     -> Exp r TFA.Bol
  ConF  :: Float    -> Exp r TFA.Flt
  AppV  :: HasSin TFG.Typ t =>
           Var r t  -> Env (Exp r) (TFG.Arg t) -> Exp r (TFG.Out t)
  Cnd   :: Exp r TFA.Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r t -> Exp r TFA.Bol) -> (Exp r t -> Exp r t) -> 
           Exp r t  -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (TFA.Tpl tf ts)
  Fst   :: HasSin TFG.Typ ts => 
           Exp r (TFA.Tpl tf ts)-> Exp r tf
  Snd   :: HasSin TFG.Typ tf => 
           Exp r (TFA.Tpl tf ts)-> Exp r ts
  Ary   :: Exp r TFA.Int -> (Exp r TFA.Int -> Exp r t) -> Exp r (TFA.Ary t)
  Len   :: HasSin TFG.Typ ta => 
           Exp r (TFA.Ary ta) -> Exp r TFA.Int
  Ind   :: Exp r (TFA.Ary ta) -> Exp r TFA.Int -> Exp r ta
  Let   :: HasSin TFG.Typ tl => 
           Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Cmx   :: Exp r TFA.Flt -> Exp r TFA.Flt -> Exp r TFA.Cmx
  Tmp   :: String -> Exp r t  -- dummy constructor

deriving instance Show (Exp r t)

ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

instance Show (Exp r ta -> Exp r tb) where
  show f =  
    
    let i = unsafePerformIO (do j <- readIORef ref
                                modifyIORef ref (+1)
                                return j)
        v = ("_x" ++ show i)    
    in ("(\\ "++ v ++ " -> (" ++ 
        show (f (Tmp v)) 
        ++ "))")

instance Show (Env (Exp r) r') where
  show Emp        = "[]"
  show (Ext x xs) = "(" ++ show x ++ ") , " ++ show xs 
 
sucAll :: Exp r t' -> Exp (t ': r) t' 
sucAll = mapVar Suc prd
                           
prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: (forall t'. Var r  t' -> Var r' t') -> 
          (forall t'. Var r' t' -> Var r  t') -> 
          Exp r t -> Exp r' t
mapVar _ _ (ConI i)       = ConI i
mapVar _ _ (ConB i)       = ConB i
mapVar _ _ (ConF i)       = ConF i
mapVar f g (AppV v es)    = AppV (f v) (ET.fmap (mapVar f g) es)
mapVar f g (Cnd ec et ef) = Cnd (mapVar f g ec) (mapVar f g et) (mapVar f g ef)
mapVar f g (Whl ec eb ei) = Whl (mapVar f g . ec . mapVar g f) 
                                (mapVar f g . eb . mapVar g f) (mapVar f g ei)
mapVar f g (Tpl ef es)    = Tpl (mapVar f g ef) (mapVar f g es)
mapVar f g (Fst e)        = Fst (mapVar f g e)
mapVar f g (Snd e)        = Snd (mapVar f g e)
mapVar f g (Ary el ef)    = Ary (mapVar f g el) (mapVar f g . ef . mapVar g f)
mapVar f g (Len e)        = Len (mapVar f g e)
mapVar f g (Ind ea ei)    = Ind (mapVar f g ea) (mapVar f g ei)
mapVar f g (Let el eb)    = Let (mapVar f g el) (mapVar f g . eb . mapVar g f)
mapVar f g (Cmx er ei)    = Cmx (mapVar f g er) (mapVar f g ei)
mapVar _ _ (Tmp x)        = Tmp x  

mapC :: r ~ TFG.Arg tt => 
        TFG.Typ tt -> (forall t. HasSin TFG.Typ t => tfa t -> tfb t) -> 
        Env tfa r -> Env tfb r
mapC _              _ Emp        = Emp
mapC (TFG.Arr t ts) f (Ext x xs) = case getPrfHasSin t of
  PrfHasSin -> Ext (f x) (mapC ts f xs)           
mapC _              _ _          = impossible

mapMC :: (Monad m , r ~ TFG.Arg tt) => 
        TFG.Typ tt -> (forall t. HasSin TFG.Typ t => tfa t -> m (tfb t)) -> 
        Env tfa r -> m (Env tfb r)
mapMC _              _ Emp        = return (Emp)
mapMC (TFG.Arr t ts) f (Ext x xs) = case getPrfHasSin t of
  PrfHasSin -> do x'  <- f x
                  xs' <- mapMC ts f xs  
                  return (Ext x' xs')
mapMC _              _ _          = impossibleM
 
absTmp :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) => 
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TFG.Typ t in case ee of 
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i  
  AppV v es                 -> AppV v (mapC (sinTyp v) (absTmp xx s) es)
  Cnd ec et ef              -> Cnd (absTmp xx s ec)   (absTmp xx s et)   
                                    (absTmp xx s ef)
  Whl ec eb ei              -> Whl (absTmp xx s . ec) (absTmp xx s . eb) 
                                   (absTmp xx s ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of 
    (PrfHasSin , PrfHasSin) -> Tpl (absTmp xx s ef)   (absTmp xx s es)
  Fst e                     -> Fst (absTmp xx s e)
  Snd e                     -> Snd (absTmp xx s e)
  Ary el ef                 -> case TFG.getPrfHasSinAry t of 
    PrfHasSin               -> Ary (absTmp xx s el)   (absTmp xx s . ef)
  Len e                     -> Len (absTmp xx s e)
  Ind ea ei                 -> Ind (absTmp xx s ea)   (absTmp xx s ei)
  Let el eb                 -> Let (absTmp xx s el)   (absTmp xx s . eb)
  Cmx er ei                 -> Cmx (absTmp xx s er)   (absTmp xx s ei) 
  Tmp x 
    | s == x                -> case eqlSin (sinTyp xx) t of  
      Rgt Rfl               -> xx
      _                     -> ee
    | otherwise             -> ee