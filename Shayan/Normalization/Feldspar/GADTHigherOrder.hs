module Normalization.Feldspar.GADTHigherOrder () where

import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTHigherOrder hiding (ref)
   
import Normalization
import Data.IORef
import System.IO.Unsafe
import Singleton
import qualified Type.Feldspar.GADT as TFG

instance HasSin TFG.Typ t => NrmOne (Exp n t) where
  nrmOne ee = let t = sin :: TFG.Typ t in case ee of
    ConI i                    -> ConI <$@> i 
    ConB b                    -> ConB <$@> b
    ConF b                    -> ConF <$@> b    
    Var x                     -> Var  <$@> x
    Abs eb                    -> case TFG.getPrfHasSinArr t of 
      (PrfHasSin , PrfHasSin) -> Abs  <$@> eb
    App (Abs eb)       ea     -> chg (eb ea)
    App (Cnd ec et ef) ea     -> chg (Cnd ec (App et ea) (App ef ea))
--    App eff (Cnd ec et ef)    -> chg (Cnd ec (App eff et) (App eff ef))
    App ef             ea     -> App  <$@> ef <*@> ea 
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