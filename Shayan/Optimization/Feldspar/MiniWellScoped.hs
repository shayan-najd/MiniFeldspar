module Optimization.Feldspar.MiniWellScoped () where

import Prelude ()
import MyPrelude
-- import Data.Constraint
-- import Data.Constraint.Unsafe
 
import Expression.Feldspar.MiniWellScoped hiding (ref)
   
import Environment.Typed hiding (add)
-- import Variable.Typed
import Optimization
import Data.IORef
import System.IO.Unsafe
import Singleton
import qualified Type.Feldspar.GADT as TFG
import qualified Conversion as C
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()
import qualified Expression.Feldspar.GADTValue as FGV

{-
add :: Env t ra -> Env t rb -> Env t (Add ra rb)
add Emp        ys = ys
add (Ext x xs) ys = Ext x (add xs ys)
 
cmt :: forall t ra rb r.(HasSin TFG.Typ t , TFG.Arg t ~ Add ra rb) => 
     Var r t -> Env (Exp r) ra -> Env (Exp r) rb -> Chg (Exp r (TFG.Out t)) 
cmt v xss ys = case ys of
  Emp                      -> return (AppV v (add xss ys))
  (Ext (Cnd ec et ef) xs)  -> chg (Cnd ec (AppV v (add xss (Ext et xs))) 
                                          (AppV v (add xss (Ext ef xs))))
  (Ext (x :: Exp r tx) (xs :: Env (Exp r) txs)) -> 
    case unsafeCoerceConstraint :: () :- 
                                   (Add (Add ra (tx ': '[])) txs 
                                    ~ Add ra rb) of
      Sub Dict -> cmt v (add xss (Ext x Emp)) xs 

hasCnd :: Env (Exp r) r' -> Bool
hasCnd Emp                  = False
hasCnd (Ext (Cnd _ _ _) _ ) = True
hasCnd (Ext _           xs) = hasCnd xs

-}

isLit :: Exp n t -> Bool
isLit (ConI _) = True
isLit (ConB _) = True
isLit (ConF _) = True
isLit _        = False

allLit :: Env (Exp r) r' -> Bool
allLit Emp                  = True
allLit (Ext x           xs) = isLit x && allLit xs

instance HasSin TFG.Typ t => OptOne (Exp r t) (Env FGV.Exp r) where
  optOne ee r = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    ConI i                    -> ConI <$@> i 
    ConB b                    -> ConB <$@> b
    ConF f                    -> ConF <$@> f    
    AppV v es 
      | allLit es             -> let Rgt (FGV.Exp e) = C.cnv (ee , r)
                                 in  chg (frmRgt (C.cnv (FGV.Exp e , r)))
      | otherwise             -> AppV v <$> optOneEnv (sinTypOf v t , es , r)
    Cnd (ConB True)  et _     -> chg et
    Cnd (ConB False) _  ef    -> chg ef
    Cnd ec et ef              -> Cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei              -> Whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es                 -> case TFG.getPrfHasSinTpl t of 
      (PrfHasSin , PrfHasSin) -> Tpl  <$@> ef <*@> es
    Fst (Tpl x _)             -> chg  x
    Fst (Cnd ec et ef)        -> chg (Cnd ec (Fst et) (Fst ef))
    Fst e                     -> Fst  <$@> e
    Snd (Tpl _ x)             -> chg  x
    Snd (Cnd ec et ef)        -> chg (Cnd ec (Snd et) (Snd ef))
    Snd e                     -> Snd  <$@> e                       
    Ary el ef                 -> case TFG.getPrfHasSinAry t of 
      PrfHasSin               -> Ary  <$@> el <*@> ef
    Len (Ary el _)            -> chg  el
    Len (Cnd ec et ef)        -> chg (Cnd ec (Len et) (Len ef))
    Len e                     -> Len  <$@> e                         
    Ind (Ary _ ef)     ei     -> chg  (ef ei)
    Ind (Cnd ec et ef) ei     -> chg (Cnd ec (Ind et ei) (Ind ef ei))    
    Ind ea             ei     -> Ind  <$@> ea <*@> ei                         
    Let el eb                 -> chg (eb el)
    Cmx er ei                 -> Cmx  <$@> er <*@> ei
    Tmp x                     -> pure (Tmp x)
    
ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

instance (HasSin TFG.Typ tb, HasSin TFG.Typ ta) => 
         OptOne (Exp r ta -> Exp r tb) (Env FGV.Exp r) where
  optOne f r = let i = unsafePerformIO (do j <- readIORef ref
                                           modifyIORef ref (+1)
                                           return j)
                   v = ("_xn" ++ show i)    
               in do eb <- optOne (f (Tmp v)) r
                     return (\ x -> absTmp x v eb) 
                   
optOneEnv :: (TFG.Arg t ~ r') => 
               (TFG.Typ t , Env (Exp r) r' , Env FGV.Exp r) -> Chg (Env (Exp r) r')
optOneEnv (TFG.Arr t ts , Ext e es , r) = case getPrfHasSin t of
    PrfHasSin -> do e'  <- optOne e r
                    es' <- optOneEnv (ts , es , r)
                    pure (Ext e' es')
optOneEnv (TFG.Arr _ _  , _       , _) = impossibleM                    
optOneEnv (_            , Emp     , _) = pure Emp                    
optOneEnv (_            , Ext _ _ , _) = impossibleM                    

