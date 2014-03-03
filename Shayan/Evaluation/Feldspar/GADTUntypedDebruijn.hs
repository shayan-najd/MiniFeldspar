module Evaluation.Feldspar.GADTUntypedDebruijn where

import Evaluation 
import Expression.Feldspar.GADTUntypedDebruijn
import qualified Expression.Feldspar.ADTValue as V
import qualified Data.Vector                  as E

type instance Val (Exp n)  = V.Val
type instance Env (Exp n)  = E.Vec n V.Val 

instance Evl (Exp n) where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    ConI i       -> V.conI <$> pure i
    ConB b       -> V.conB <$> pure b 
    Var x        -> (return . return) (E.get x r)
    Abs eb       -> V.abs <$> pure (frmRgt . evl eb . (E.::: r))
    App ef ea    -> V.app <$@> ef <*@> ea 
    Cnd ec et ef -> V.cnd <$@> ec <*@> et <*@> ef      
    Tpl ef es    -> V.tpl <$@> ef <*@> es 
    Fst e        -> V.fst <$@> e
    Snd e        -> V.snd <$@> e 
    Ary el ef    -> V.arr <$@> el <*@> ef
    Len e        -> V.len <$@> e                         
    Ind ea ei    -> V.ind <$@> ea <*@> ei                         
    Whl ec eb ei -> V.whl <$@> ec <*@> eb <*@> ei
    Let el eb    -> return (evlr (App (Abs eb) el))
    where 
      evlr :: Exp n -> ErrM V.Val
      evlr e = evl e r
      
      