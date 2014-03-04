module Evaluation.Feldspar.ADTUntypedDebruijn where

import Evaluation 
import Expression.Feldspar.ADTUntypedDebruijn
import qualified Expression.Feldspar.ADTValue as V
import qualified Environment.ADT as E
 
type instance Val Exp  = V.Val
type instance Env Exp  = E.Env V.Val 

instance Evl Exp where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    ConI i       -> V.conI <$> pure i
    ConB b       -> V.conB <$> pure b 
    Var x        -> return (E.get x r)
    Abs eb       -> V.abs <$> evlrf eb
    App ef ea    -> V.app <$@> ef <*@> ea 
    Cnd ec et ef -> V.cnd <$@> ec <*@> et <*@> ef      
    Whl ec eb ei -> V.whl <$> evlrf ec <*> evlrf eb <*@> ei
    Tpl ef es    -> V.tpl <$@> ef <*@> es 
    Fst e        -> V.fst <$@> e
    Snd e        -> V.snd <$@> e 
    Ary el ef    -> V.ary <$@> el <*> evlrf ef 
    Len e        -> V.len <$@> e                         
    Ind ea ei    -> V.ind <$@> ea <*@> ei                         
    Let el eb    -> return (evlr (App (Abs eb) el))
    where 
      evlr :: Exp -> ErrM V.Val
      evlr e = evl e r
      
      evlrf :: Exp -> ErrM (V.Val -> V.Val)
      evlrf e = pure (frmRgt . evl e . (: r)) 