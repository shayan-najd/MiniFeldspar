{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,ScopedTypeVariables, FlexibleInstances #-}
module Evaluation.Feldspar.GADTHigherOrder where

import Evaluation 
import Evaluation.Variable.GADT ()
import Expression.Feldspar.GADTHigherOrder
import qualified Value.Feldspar.GADT as V
import Control.Applicative.Recursion
import ErrorMonad
import Data.Array
import Data.Maybe(fromJust)
import Singleton 
import qualified Singleton.TypeFeldspar as G  
import Singleton.TypeFeldspar ()
import Singleton.Environment(get)

type instance Val (Exp r t) = Trm t
type instance Env (Exp r t) = Trm r
type instance Val (G.Typ ta , Exp r ta -> Exp r tb) = Trm ta -> Trm tb
type instance Env (G.Typ ta , Exp r ta -> Exp r tb) = Trm r

instance Evl (Exp r t) where
  evl egfo r = case egfo of 
       ConI i       -> V.conI <$> pure i
       ConB b       -> V.conB <$> pure b
       Var x        -> return (get x r)
       App ef ea    -> V.app  <$> evl ef r <*> evl ea r
       Abs t eb     -> evl (t , eb) r
       Cnd ec et ef -> V.cnd  <$> evl ec r <*> evl et r <*> evl ef r
       Tpl ef es    -> V.tpl  <$> evl ef r <*> evl es r
       Fst e        -> V.fst  <$> evl e  r
       Snd e        -> V.snd  <$> evl e  r                      
       Ary el ef    -> V.arr  <$> evl el r <*> evl ef r
       Len e        -> V.len  <$> evl e  r                        
       Ind ea ei    -> V.ind  <$> evl ea r <*> evl ei r                        
       Whl ec eb ei -> V.whl  <$> evl ec r <*> evl eb r <*> evl ei r
       Let t el eb  -> evl (App (Abs t eb) el) r
 
instance Evl (G.Typ ta , Exp r ta -> Exp r tb) where 
  evl (t , f) r =  pure (frmRgt . flip evl r . f . revEvl t r)

revEvl :: G.Typ t -> Trm r -> Trm t -> Exp r t
revEvl G.Int         _ i         = ConI i
revEvl G.Bol         _ b         = ConB b  
revEvl (G.Arr ta tb) r f         = Abs ta (revEvl tb r . f . frmRgt 
                                           . flip evl r)
revEvl (G.Tpl tf ts) r (vf , vs) = Tpl (revEvl tf r vf) (revEvl ts r vs)
revEvl (G.Ary ta)    r va        = let (0,l) = bounds va in
  Ary (ConI l) (revEvl (G.Arr G.Int ta) r 
                (fromJust . flip lookup (assocs va)))
  