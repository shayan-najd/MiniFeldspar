module Evaluation.Feldspar.ADTChurch where

import Evaluation 
import Expression.Feldspar.ADTChurch
import qualified Expression.Feldspar.ADTValue as V
import qualified Environment.ADT              as A
 
type instance Val (Exp v)  = V.Val
type instance Env (Exp v)  = A.Env V.Val 

instance Evl (Exp v) where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    ConI i       -> V.conI <$> pure i
    ConB b       -> V.conB <$> pure b 
    Var x        -> return (A.get x r)
    Abs _ eb     -> V.abs <$> pure (frmRgt . evl eb . (: r))
    App ef ea    -> V.app <$@> ef <*@> ea 
    Cnd ec et ef -> V.cnd <$@> ec <*@> et <*@> ef      
    Tpl ef es    -> V.tpl <$@> ef <*@> es 
    Fst e        -> V.fst <$@> e
    Snd e        -> V.snd <$@> e 
    Ary el ef    -> V.arr <$@> el <*@> ef
    Len e        -> V.len <$@> e                         
    Ind ea ei    -> V.ind <$@> ea <*@> ei                         
    Whl ec eb ei -> V.whl <$@> ec <*@> eb <*@> ei
    Let el eb    -> return (evlr (App (Abs undefined eb) el))
    where 
      evlr :: Exp v -> ErrM V.Val
      evlr e = evl e r
      
      