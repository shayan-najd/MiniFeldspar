module Evaluation.Feldspar.ADTUntypedNamed where

import Evaluation 
import Expression.Feldspar.ADTUntypedNamed
import qualified Expression.Feldspar.ADTValue as V
import qualified Environment.ADTTable as E
  
type instance Val (Exp v)  = V.Val
type instance Env (Exp v)  = E.Env v V.Val 

instance Eq v => Evl (Exp v) where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    ConI i       -> V.conI <$> pure i
    ConB b       -> V.conB <$> pure b 
    Var x        -> return (E.get x r)
    Abs x eb     -> V.abs <$> pure (frmRgt . evl eb . (: r) . (,) x)
    App ef ea    -> V.app <$@> ef <*@> ea 
    Cnd ec et ef -> V.cnd <$@> ec <*@> et <*@> ef      
    Tpl ef es    -> V.tpl <$@> ef <*@> es 
    Fst e        -> V.fst <$@> e
    Snd e        -> V.snd <$@> e 
    Ary el ef    -> V.arr <$@> el <*@> ef
    Len e        -> V.len <$@> e                         
    Ind ea ei    -> V.ind <$@> ea <*@> ei                         
    Whl ec eb ei -> V.whl <$@> ec <*@> eb <*@> ei
    Let x el eb  -> return (evlr (App (Abs x eb) el))
    where 
      evlr :: Exp v -> ErrM V.Val
      evlr e = evl e r