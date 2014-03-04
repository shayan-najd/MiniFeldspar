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
    Whl ec eb ei -> V.whl <$> pure (frmRgt . evl ec . (E.::: r)) 
                          <*> pure (frmRgt . evl eb . (E.::: r))  <*@> ei
    Tpl ef es    -> V.tpl <$@> ef <*@> es 
    Fst e        -> V.fst <$@> e
    Snd e        -> V.snd <$@> e 
    Ary el ef    -> V.ary <$@> el <*> pure (frmRgt . evl ef . (E.::: r))
    Len e        -> V.len <$@> e                         
    Ind ea ei    -> V.ind <$@> ea <*@> ei                         
    Let el eb    -> return (evlr (App (Abs eb) el))
    where 
      evlr :: (Evl e , Env e ~ E.Vec n V.Val ) => e -> ErrM (Val e)
      evlr e = evl e r
      

      
      