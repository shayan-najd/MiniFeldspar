{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,FlexibleInstances, ScopedTypeVariables
           , TypeOperators, DataKinds, PolyKinds, FlexibleContexts #-}
module Evaluation.Feldspar.MiniWellScoped where

import Prelude hiding (sin)
import Expression.Feldspar.MiniWellScoped
import Evaluation 
import ErrorMonad 
import Singleton
import Control.Applicative.Recursion

import Data.Array
import Data.Maybe(fromJust)

import qualified Type.Feldspar                 as A
import qualified Singleton.Environment         as G
import qualified Singleton.TypeFeldspar        as F
import qualified Expression.Feldspar.GADTValue as V

type instance Val (Exp r t) = Trm t
type instance Env (Exp r t) = Trm r

type instance Val (F.Typ t , Exp r t) = Trm t
type instance Env (F.Typ t , Exp r t) = Trm r

instance HasSin F.Typ t => Evl (Exp r t) where
  evl e r = evl (sin :: F.Typ t , e) r

instance Evl (F.Typ t , Exp r t) where
  evl (t , egfo) r = case egfo of 
       ConI i          -> V.conI <$> pure i
       ConB b          -> V.conB <$> pure b
       AppV tv v es    -> appVec tv (G.get v r) es r
       Cnd ec et ef    -> V.cnd  <$> evl (F.Bol , ec) r 
                          <*> evl (t , et) r <*> evl (t , ef) r       
       Tpl ef es       -> case t of
         F.Tpl tf ts   -> V.tpl  <$> evl (tf , ef) r <*> evl (ts , es) r
         _             -> fail "Impossible!"
       Fst ts e        -> V.fst  <$> evl (F.Tpl t  ts , e)  r
       Snd tf e        -> V.snd  <$> evl (F.Tpl tf t  , e)  r
       Ary el ef       -> case t of
         F.Ary ta      -> V.arr  <$> evl (F.Int , el) r 
                          <*> evl (F.Arr F.Int ta, ef) r
         _             -> fail "Impossible!"                          
       Len ta e        -> V.len  <$> evl (F.Ary ta , e)  r                       
       Ind ea ei       -> V.ind  <$> evl (F.Ary t , ea) r <*> evl (F.Int , ei) r
       Whl ec eb ei    -> V.whl  <$> evl (F.Arr t F.Bol , ec) r 
                                 <*> evl (F.Arr t t , eb) r 
                                 <*> evl (t , ei) r
       Let tl el eb    -> V.lett <$> evl (tl , el) r <*> evl (F.Arr tl t , eb) r

appVec :: F.Typ t -> Trm t -> G.Env (Exp r) (Arg t) -> Trm r -> 
          ErrM (Trm (Out t))
appVec (F.Arr ta  tb) vf (G.Ext e es) r = do v <- evl (ta , e) r
                                             appVec tb (vf v) es r
appVec F.Int         vi G.Emp         _ = return vi
appVec F.Bol         vb G.Emp         _ = return vb
appVec (F.Tpl _  _ ) vt G.Emp         _ = return vt
appVec (F.Ary _    ) va G.Emp         _ = return va
appVec _             _  _             _ = fail "Impossible!"
 
type instance Val (Exp r ta -> Exp r tb) = Trm ta -> Trm tb
type instance Env (Exp r ta -> Exp r tb) = Trm r

type instance Val (F.Typ (A.Arr ta tb) , Exp r ta -> Exp r tb) = Trm ta -> Trm tb
type instance Env (F.Typ (A.Arr ta tb) , Exp r ta -> Exp r tb) = Trm r

instance HasSin F.Typ (A.Arr ta  tb) => Evl (Exp r ta -> Exp r tb) where
  evl e  r = evl (sin :: F.Typ (A.Arr ta  tb) , e) r 

instance Evl (F.Typ (A.Arr ta  tb), Exp r ta -> Exp r tb) where
  evl (F.Arr ta tb , f) r = return (frmRgt . (\ e -> evl (tb , e) r) . f . 
                                    revEvl ta r ) 
  
revEvl :: F.Typ t -> Trm r -> Trm t -> Exp r t
revEvl F.Int         _ i         = ConI i
revEvl F.Bol         _ b         = ConB b  
revEvl (F.Tpl tf ts) r (vf , vs) = Tpl (revEvl tf r vf) (revEvl ts r vs)
revEvl (F.Ary ta)    r va        = let (0,l) = bounds va in
  Ary (ConI l) (revEvl ta r . fromJust . flip lookup (assocs va) 
                . frmRgt 
                . flip evl r)
revEvl _             _  _        = error "Bad Input!"  
    