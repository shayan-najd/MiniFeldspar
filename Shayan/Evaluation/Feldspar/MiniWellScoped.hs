{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,FlexibleInstances, ScopedTypeVariables
           , TypeOperators, DataKinds, PolyKinds #-}
module Evaluation.Feldspar.MiniWellScoped where

import Expression.Feldspar.MiniWellScoped
import Evaluation 
import ErrorMonad 
import Singleton
import Control.Applicative.Recursion

import Data.Array
import Data.Maybe(fromJust)

import qualified Type.Feldspar          as A
import qualified Singleton.Environment  as G
import qualified Singleton.TypeFeldspar as F
import qualified Value.Feldspar.GADT    as V

type instance Val (Exp r t) = Trm t
type instance Env (Exp r t) = Trm r

instance Evl (Exp r t) where
  evl egfo r = case egfo of 
       ConI i          -> V.conI <$> pure i
       ConB b          -> V.conB <$> pure b
       AppV tv v es    -> appVec tv (G.get v r) 
                          <$> G.mapM ((Idn <$>) . flip evl r) es 
       Cnd ec et ef    -> V.cnd  <$> evl ec r <*> evl et r <*> evl ef r       
       Tpl ef es       -> V.tpl  <$> evl ef r <*> evl es r
       Fst e           -> V.fst  <$> evl e  r
       Snd e           -> V.snd  <$> evl e  r                     
       Ary el ef       -> V.arr  <$> evl el r <*> evl (F.Int , ef) r
       Len e           -> V.len  <$> evl e  r                       
       Ind ea ei       -> V.ind  <$> evl ea r <*> evl ei r
       Whl t ec eb ei  -> V.whl  <$> evl (t , ec) r <*> evl (t , eb) r 
                                 <*> evl ei r
       Let t el eb     -> V.lett <$> evl el r <*> evl (t , eb) r         
       
data Idn (t :: A.Typ) = Idn {frmIdn :: Trm t}  
 
appVec :: F.Typ t -> Trm t -> G.Env Idn (Arg t)-> Trm (Out t)
appVec (F.Arr _  tb) vf (G.Ext v vs) = appVec tb (vf (frmIdn v)) vs 
appVec F.Int         vi G.Emp        = vi
appVec F.Bol         vb G.Emp        = vb
appVec (F.Tpl _  _ ) vt G.Emp        = vt
appVec (F.Ary _    ) va G.Emp        = va
appVec _             _  _            = error "Impossible!"
 
type instance Val (F.Typ ta , Exp r ta -> Exp r tb) = Trm ta -> Trm tb
type instance Env (F.Typ ta , Exp r ta -> Exp r tb) = Trm r

instance Evl (F.Typ ta , Exp r ta -> Exp r tb) where
  evl (t , f) r = return (frmRgt . flip evl r . f . revEvl t r ) 
  
revEvl :: F.Typ t -> Trm r -> Trm t -> Exp r t
revEvl F.Int         _ i         = ConI i
revEvl F.Bol         _ b         = ConB b  
revEvl (F.Tpl tf ts) r (vf , vs) = Tpl (revEvl tf r vf) (revEvl ts r vs)
revEvl (F.Ary ta)    r va        = let (0,l) = bounds va in
  Ary (ConI l) (revEvl ta r . fromJust . flip lookup (assocs va) 
                . frmRgt 
                . flip evl r)
revEvl _             _  _        = error "Bad Input!"  
    