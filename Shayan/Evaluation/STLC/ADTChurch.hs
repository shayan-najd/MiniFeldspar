module Evaluation.STLC.ADTChurch where

import Evaluation 
import Expression.STLC.ADTChurch
import qualified Expression.STLC.ADTValue as V
import qualified Environment.ADT          as E
  
type instance Val (Exp t) = V.Val
type instance Env (Exp t) = E.Env V.Val 
 
instance Evl (Exp t) where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    Con i        -> V.con <$> pure i
    Var x        -> return (E.get x r)
    Abs _ eb     -> V.abs <$> pure (frmRgt . evl eb . (: r))
    App ef ea    -> V.app <$@> ef <*@> ea 
    Add el er    -> V.add <$@> el <*@> er 
    where 
      evlr :: Exp t -> ErrM V.Val
      evlr e = evl e r  