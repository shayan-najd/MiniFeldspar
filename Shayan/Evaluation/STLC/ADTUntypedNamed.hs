module Evaluation.STLC.ADTUntypedNamed where

import Evaluation 
import Expression.STLC.ADTUntypedNamed
import qualified Expression.STLC.ADTValue as V
import qualified Environment.ADTTable     as E
 
type instance Val (Exp v)  = V.Val
type instance Env (Exp v)  = E.Env v V.Val 

instance Eq v => Evl (Exp v) where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    Con i        -> V.con <$> pure i
    Var x        -> return (E.get x r)
    Abs x eb     -> V.abs <$> pure (frmRgt . evl eb . (: r) . (,) x)
    App ef ea    -> V.app <$@> ef <*@> ea 
    Add el er    -> V.add <$@> el <*@> er 
    where 
      evlr :: Exp v -> ErrM V.Val
      evlr e = evl e r