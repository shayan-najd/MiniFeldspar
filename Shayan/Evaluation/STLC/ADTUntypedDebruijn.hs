module Evaluation.STLC.ADTUntypedDebruijn where

import Evaluation 
import Expression.STLC.ADTUntypedDebruijn
import qualified Expression.STLC.ADTValue as V
import qualified Environment.ADT          as E
 
type instance Val Exp = V.Val
type instance Env Exp = E.Env V.Val 

instance Evl Exp where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    Con i        -> V.con <$> pure i
    Var x        -> return (E.get x r)
    Abs eb       -> V.abs <$> pure (frmRgt . evl eb . (: r))
    App ef ea    -> V.app <$@> ef <*@> ea 
    Add el er    -> V.add <$@> el <*@> er 
    where 
      evlr :: Exp -> ErrM V.Val
      evlr e = evl e r