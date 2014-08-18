module Normalization.Feldspar.GADTHigherOrder (eta) where

import MyPrelude

import Expression.Feldspar.GADTHigherOrder hiding (ref)

import Normalization
import Data.IORef
import System.IO.Unsafe
import Singleton
import qualified Type.Feldspar.GADT as TFG

isVal :: Exp n t -> Bool
isVal ee = case ee of
    ConI _       -> True
    ConB _       -> True
    ConF _       -> True
    Var  _       -> True
    Abs  _       -> True
    App  _  _    -> False
    Cnd  _  _  _ -> False
    Whl  _  _  _ -> False
    Tpl  ef es   -> isVal ef && isVal es
    Fst  _       -> False
    Snd  _       -> False
    Ary  el _    -> isVal el
    Len  _       -> False
    Ind  _  _    -> False
    Let  _  _    -> False
    Cmx  er ei   -> isVal er && isVal ei
    Tmp  _       -> True

val :: Exp n t -> (Bool,Exp n t)
val ee = (isVal ee , ee)

pattern V  v <- (val -> (True  , v))
pattern NV v <- (val -> (False , v))

eta :: forall n t. HasSin TFG.Typ t =>
       Exp n t -> Exp n t
eta e = let t = sin :: TFG.Typ t in
  case e of
    Abs eb -> case TFG.getPrfHasSinArr t of
        (PrfHasSin , PrfHasSin) -> Abs (etaF eb)
    _      -> case t of
      TFG.Arr _ _               -> case TFG.getPrfHasSinArr t of
        (PrfHasSin , PrfHasSin) -> Abs (\ x -> eta (App e x))
      _                         -> etasub e

etasub :: forall n t. HasSin TFG.Typ t =>
          Exp n t -> Exp n t
etasub ee = let t = sin :: TFG.Typ t in case ee of
    ConI i                    -> ConI i
    ConB b                    -> ConB b
    ConF f                    -> ConF f
    Var x                     -> Var  x
    Abs eb                    -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin) -> Abs (etaF eb)
    App ef             ea     -> App (etasub ef) (eta ea)
    Cnd ec et ef              -> Cnd (eta ec) (eta et) (eta ef)
    Whl ec eb ei              -> Whl (etaF ec) (etaF eb) (eta ei)
    Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin) -> Tpl (eta ef) (eta es)
    Fst e                     -> Fst (eta e)
    Snd e                     -> Snd (eta e)
    Ary el ef                 -> case TFG.getPrfHasSinAry t of
      PrfHasSin               -> Ary (eta el) (etaF ef)
    Len e                     -> Len (eta e)
    Ind ea             ei     -> Ind (eta ea) (eta ei)
    Let el eb                 -> Let (eta el) (etaF eb)
    Cmx er ei                 -> Cmx (eta er) (eta ei)
    Tmp x                     -> Tmp x

etaF :: forall n ta tb. (HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
           (Exp n ta -> Exp n tb) -> (Exp n ta -> Exp n tb)
etaF f = let i  = unsafePerformIO (do j <- readIORef ref
                                      modifyIORef ref (+1)
                                      return j)
             v  = "_xn" ++ show i
             eb = eta (f (Tmp v))
         in (\ x -> absTmp x v eb)

instance HasSin TFG.Typ t => NrmOne (Exp n t) where
  nrmOne ee = let t = sin :: TFG.Typ t in case ee of
    ConI i                       -> pure (ConI i)
    ConB b                       -> pure (ConB b)
    ConF f                       -> pure (ConF f)
    Var x                        -> pure (Var  x)
    Abs eb                       -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin)    -> Abs  <$@> eb
    App ef             (NV ea)   -> chg (Let ea (\ x -> App ef     x ))
    App (Abs eb)       (V  ea)   -> chg (eb ea)
    App (Cnd (V ec) et ef)(V ea) -> chg (Cnd ec (App et ea) (App ef ea))
    App (Let (NV el) eb) (V ea)  -> chg (Let el (\ x -> App (eb x) ea))
    App ef             ea        -> App  <$@> ef <*@> ea

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