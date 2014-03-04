module Evaluation.Feldspar.GADTFirstOrder where

import Prelude hiding (sin)
import Evaluation 
import Expression.Feldspar.GADTFirstOrder
import qualified Expression.Feldspar.GADTValue as V
import Singleton 
import qualified Singleton.Environment as E
import qualified Singleton.TypeFeldspar as G
import Evaluation.Variable ()
  
type instance Val (Exp r t) = Trm t
type instance Env (Exp r t) = Trm r
type instance Val (G.Typ t , Exp r t) = Trm t
type instance Env (G.Typ t , Exp r t) = Trm r

instance HasSin G.Typ t => Evl (Exp r t) where
  evl e r = evl (sin :: G.Typ t , e) r 
  
instance Evl (G.Typ t , Exp r t) where
  evl (t , egfo) r = case egfo of 
    ConI i       -> V.conI <$> pure i
    ConB b       -> V.conB <$> pure b
    Var x        -> return (E.get x r)
    Abs eb       -> case t of
     G.Arr _  tb -> V.abs  <$> pure (\ va -> frmRgt (evl (tb , eb) (va , r)))
     _           -> fail "Impossible!"
    App ta ef ea -> V.app  <$> evl (G.Arr ta t , ef) r <*> evl (ta , ea) r
    Cnd ec et ef -> V.cnd  <$> evl (G.Bol , ec) r <*> evl (t , et) r 
                    <*> evl (t , ef) r
    Whl ec eb ei -> V.whl  
                    <$> pure (\ va -> frmRgt (evl (G.Bol , ec) (va , r))) 
                    <*> pure (\ va -> frmRgt (evl (t     , eb) (va , r))) 
                    <*> evl (t , ei) r
    Tpl ef es    -> case t of
     G.Tpl tf ts -> V.tpl  <$> evl (tf , ef) r <*> evl (ts , es) r
     _           -> fail "Impossible!"
    Fst ts e     -> V.fst  <$> evl (G.Tpl t ts , e)  r
    Snd tf e     -> V.snd  <$> evl (G.Tpl tf t , e)  r                      
    Ary el ef    -> case t of 
     G.Ary ta    -> V.ary  <$> evl (G.Int , el) r 
                    <*>  pure (\ va -> frmRgt (evl (ta , ef) (va , r)))
     _           -> fail "Impossible!"     
    Len ta e     -> V.len  <$> evl (G.Ary ta , e)  r                       
    Ind ea ei    -> V.ind  <$> evl (G.Ary t , ea) r <*> evl (G.Int , ei) r
    Let tl el eb -> evl (t , (App tl (Abs eb) el)) r 