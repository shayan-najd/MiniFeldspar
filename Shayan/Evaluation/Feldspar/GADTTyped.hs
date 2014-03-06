module Evaluation.Feldspar.GADTTyped where

import Evaluation 
import Expression.Feldspar.GADTTyped
import qualified Expression.Feldspar.ADTValue as V
import qualified Data.Vector                  as A
import qualified Data.Nat                     as A
 
type instance Val (Exp n t) = V.Val
type instance Env (Exp n t) = A.Vec n V.Val 
 
instance Evl (Exp n t) where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    ConI i       -> V.conI <$> pure i
    ConB b       -> V.conB <$> pure b 
    Var x        -> (return . return) (A.get x r)
    Abs eb       -> V.abs <$> evlrf eb
    App _  ef ea -> V.app <$@> ef <*@> ea 
    Cnd ec et ef -> V.cnd <$@> ec <*@> et <*@> ef      
    Whl ec eb ei -> V.whl <$> evlrf ec <*> evlrf eb <*@> ei
    Tpl ef es    -> V.tpl <$@> ef <*@> es 
    Fst _  e     -> V.fst <$@> e
    Snd _  e     -> V.snd <$@> e 
    Ary el ef    -> V.ary <$@> el <*> evlrf ef
    Len _  e     -> V.len <$@> e                         
    Ind ea ei    -> V.ind <$@> ea <*@> ei                            
    Let _  el eb -> return (evlr (App undefined (Abs eb) el)) 
    where 
      evlr :: Exp n t -> ErrM (V.Val)
      evlr e = evl e r

      evlrf :: Exp (A.Suc n) t-> ErrM (V.Val -> V.Val)
      evlrf e = pure (frmRgt . evl e . (A.::: r))