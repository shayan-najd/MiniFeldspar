module Normalization.Feldspar.MiniWellScoped () where

import Prelude ()
import MyPrelude
import Data.Constraint
import Data.Constraint.Unsafe
 
import Expression.Feldspar.MiniWellScoped hiding (ref)
   
import Environment.Typed hiding (add)
import Variable.Typed
import Normalization
import Data.IORef
import System.IO.Unsafe
import Singleton
import qualified Type.Feldspar.GADT as TFG

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

instance HasSin TFG.Typ t => NrmOne (Exp n t) where
  nrmOne ee = let t = sin :: TFG.Typ t in case ee of
    ConI i                    -> ConI <$@> i 
    ConB b                    -> ConB <$@> b
    ConF b                    -> ConF <$@> b    
    AppV v es {-
      | hasCnd es             -> cmt v Emp es
      | otherwise     -}      -> AppV v <$> nrmOneEnv (sinTypOf v t , es) 
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
         NrmOne (Exp n ta -> Exp n tb) where
  nrmOne f = let i = unsafePerformIO (do j <- readIORef ref
                                         modifyIORef ref (+1)
                                         return j)
                 v = ("_xn" ++ show i)    
             in do eb <- nrmOne (f (Tmp v))
                   return (\ x -> absTmp x v eb) 
                   
nrmOneEnv :: (TFG.Arg t ~ r') => 
               (TFG.Typ t , Env (Exp r) r') -> Chg (Env (Exp r) r')
nrmOneEnv (TFG.Arr t ts , Ext e es) = case getPrfHasSin t of
    PrfHasSin -> do e'  <- nrmOne e
                    es' <- nrmOneEnv (ts , es)
                    pure (Ext e' es')
nrmOneEnv (TFG.Arr _ _  , _)        = impossibleM                    
nrmOneEnv (_            , Emp)      = pure Emp                    
nrmOneEnv (_            , Ext _ _)  = impossibleM                    

