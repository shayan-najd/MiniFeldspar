module Normalization.Feldspar.MiniWellscoped () where

import MyPrelude hiding (foldl,fmap)

import Expression.Feldspar.MiniWellScoped

import Normalization
import Data.IORef
import System.IO.Unsafe
import Singleton
import qualified Type.Feldspar.GADT as TFG
import Environment.Typed
import Variable.Typed

import Data.Constraint
import Data.Constraint.Unsafe


isVal :: Exp n t -> Bool
isVal ee = case ee of
  ConI _       -> True
  ConB _       -> True
  ConF _       -> True
  AppV _ Emp   -> True
  AppV _ _     -> False
  Cnd  _  _  _ -> False
  Whl  _  _  _ -> False
  Tpl  ef es   -> isVal ef && isVal es
  Fst  _       -> False
  Snd  _       -> False
  Ary  el _    -> isVal el
  Len  _       -> False
  Ind  _  _    -> False
  Let  _  _    -> False
  Cmx  _  _    -> True
  Tmp  _       -> True
  Tag  _  e    -> isVal e

val :: Exp n t -> (Bool,Exp n t)
val ee = (isVal ee , ee)

pattern V  v <- (val -> (True  , v))
pattern NV v <- (val -> (False , v))

add :: Env t ra -> Env t rb -> Env t (Add ra rb)
add Emp        ys = ys
add (Ext x xs) ys = Ext x (add xs ys)

getPrf :: (TFG.Arg t ~ Add ra (tb ': rb)) =>
          TFG.Typ t -> Env T ra -> Env T (tb ': rb) -> PrfHasSin TFG.Typ tb
getPrf TFG.Int                   _          _           = impossible
getPrf TFG.Bol                   _          _           = impossible
getPrf TFG.Flt                   _          _           = impossible
getPrf (TFG.Tpl _ _)             _          _           = impossible
getPrf (TFG.Ary _)               _          _           = impossible
getPrf TFG.Cmx                   _          _           = impossible
getPrf (TFG.Arr t TFG.Int)       Emp        (Ext T Emp) = getPrfHasSin t
getPrf (TFG.Arr _ TFG.Int)       _          _           = impossible
getPrf (TFG.Arr t TFG.Bol)       Emp        (Ext T Emp) = getPrfHasSin t
getPrf (TFG.Arr _ TFG.Bol)       _          _           = impossible
getPrf (TFG.Arr t TFG.Flt)       Emp        (Ext T Emp) = getPrfHasSin t
getPrf (TFG.Arr _ TFG.Flt)       _          _           = impossible
getPrf (TFG.Arr t (TFG.Tpl _ _)) Emp        (Ext T Emp) = getPrfHasSin t
getPrf (TFG.Arr _ (TFG.Tpl _ _)) _          _           = impossible
getPrf (TFG.Arr t (TFG.Ary _))   Emp        (Ext T Emp) = getPrfHasSin t
getPrf (TFG.Arr _ (TFG.Ary _))   _          _           = impossible
getPrf (TFG.Arr t TFG.Cmx)       Emp        (Ext T Emp) = getPrfHasSin t
getPrf (TFG.Arr _ TFG.Cmx)       _          _           = impossible
getPrf (TFG.Arr t (TFG.Arr _ _)) Emp        (Ext T _)   = getPrfHasSin t
getPrf (TFG.Arr _ ts@(TFG.Arr _ _)) (Ext T es) es'      = getPrf ts es es'

cmt :: forall t ra rb r.
       (HasSin TFG.Typ t , TFG.Arg t ~ Add ra rb) =>
       Var r t -> Env (Exp r) ra -> Env (Exp r) rb -> Chg (Exp r (TFG.Out t))
cmt v r r' = case r' of
  Emp           -> return (AppV v (add r r'))
  Ext (NV e) es -> case getPrf (sinTyp v) (fmap (\ _ -> T) r)
                   (fmap (\ _ -> T) r') of
    PrfHasSin   -> chg (Let e (\ x -> AppV v (add r (Ext x es))))
  Ext (x :: Exp r tx) (xs :: Env (Exp r) txs) ->
    case unsafeCoerceConstraint
        :: () :- (Add (Add ra (tx ': '[])) txs ~ Add ra (tx ': txs)) of
      Sub Dict  -> cmt v (add r (Ext x Emp)) xs

hasNV :: Env (Exp r) r' -> Bool
hasNV = foldl (\ b e -> b || (not (isVal e))) False

instance HasSin TFG.Typ t => NrmOne (Exp n t) where
  nrmOne ee = let t = sin :: TFG.Typ t in case ee of
    ConI i                       -> pure (ConI i)
    ConB b                       -> pure (ConB b)
    ConF f                       -> pure (ConF f)
    AppV x             es
        | hasNV es               -> cmt x Emp es
        | otherwise              -> AppV x <$> (nrmOneEnv (sinTypOf x t , es))
    Cnd (NV ec)      et ef       -> chg (Let ec (\ x -> Cnd x et ef))
    Cnd (ConB True)  et _        -> chg et
    Cnd (ConB False) _  ef       -> chg ef
    Cnd ec           et ef       -> Cnd  <$@> ec <*@> et <*@> ef

    Whl ec eb (NV ei)            -> chg (Let ei (\ x -> Whl ec eb x))
    Whl ec eb ei                 -> Whl  <$@> ec <*@> eb <*@> ei

    Tpl (NV ef) es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let ef (\ x -> Tpl x es))
    Tpl (V ef)  (NV es)          -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let es (\ x -> Tpl ef x))
    Tpl ef      es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> Tpl  <$@> ef <*@> es

    Fst (NV e)                   -> chg (Let e (\ x -> Fst x))
    Fst (Tpl (V ef) (V _))       -> chg  ef
    Fst e                        -> Fst  <$@> e

    Snd (NV e)                   -> chg (Let e (\ x -> Snd x))
    Snd (Tpl (V _)  (V es))      -> chg  es
    Snd e                        -> Snd  <$@> e

    Ary (NV el) ef               -> chg (Let el (\ x -> Ary x ef))
    Ary el      ef               -> case TFG.getPrfHasSinAry t of
      PrfHasSin                  -> Ary  <$@> el <*@> ef

    Len (NV ea)                  -> chg (Let ea (\ x -> Len x))
    Len (Ary (V el) _)           -> chg  el
    Len e                        -> Len  <$@> e

    Ind (NV ea)        ei        -> chg (Let ea (\ x -> Ind x  ei))
    Ind (V ea)         (NV ei)   -> chg (Let ei (\ x -> Ind ea x ))
    Ind (Ary (V _) ef) (V ei)    -> chg (ef ei)
    Ind ea             ei        -> Ind  <$@> ea <*@> ei

    Cmx (NV er) ei               -> chg (Let er (\ x -> Cmx  x  ei))
    Cmx (V er)  (NV ei)          -> chg (Let ei (\ x -> Cmx  er x ))
    Cmx er ei                    -> Cmx  <$@> er <*@> ei

    Let (Let (NV el') eb')  eb   -> chg (Let el' (\ x -> Let (eb' x) eb))
    Let (Cnd ec et ef)      eb   -> chg (Cnd ec (Let et eb) (Let ef eb))
    Let (V v)               eb   -> chg (eb v)
    Let (NV v)         eb
      | isFresh eb               -> chg (eb v)
    Let el             eb        -> Let  <$@> el <*@> eb
    Tmp x                        -> pure (Tmp x)
    Tag x e                      -> Tag x <$@> e

ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

instance (HasSin TFG.Typ tb, HasSin TFG.Typ ta) =>
         NrmOne (Exp n ta -> Exp n tb) where
  nrmOne f = let i = unsafePerformIO (do j <- readIORef ref
                                         modifyIORef ref (+1)
                                         return j)
                 v = "_xn" ++ show i
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