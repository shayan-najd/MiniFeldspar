module Evaluation.Feldspar.ADTUntypedNamed where

import Evaluation 
import Expression.Feldspar.ADTUntypedNamed
import qualified Expression.Feldspar.ADTValue as V
import qualified Environment.ADTTable as E
  
type instance Val (Exp v)  = V.Val
type instance Env (Exp v)  = E.Env v V.Val 

instance Eq v => Evl (Exp v) where
  evl ee r = let ?cnv = evlr in join $ case ee of        
    ConI i             -> V.conI <$> pure i
    ConB b             -> V.conB <$> pure b 
    Var x              -> return (E.get x r)
    Abs x eb           -> V.abs <$> evlrf (x , eb)
    App ef ea          -> V.app <$@> ef <*@> ea 
    Cnd ec et ef       -> V.cnd <$@> ec <*@> et <*@> ef      
    Whl xc ec xb eb ei -> V.whl <$> evlrf (xc , ec) <*> evlrf (xb , eb) <*@> ei
    Tpl ef es          -> V.tpl <$@> ef <*@> es 
    Fst e              -> V.fst <$@> e
    Snd e              -> V.snd <$@> e 
    Ary el x ef        -> V.ary <$@> el <*> evlrf (x , ef)
    Len e              -> V.len <$@> e                         
    Ind ea ei          -> V.ind <$@> ea <*@> ei                         
    Let x el eb        -> return (evlr (App (Abs x eb) el))
    where 
      evlr :: (Evl e , Env e ~ E.Env v V.Val) => e -> ErrM (Val e)
      evlr e = evl e r 
      
      evlrf :: (v , Exp v) -> ErrM (V.Val -> V.Val)
      evlrf (x , e) = pure (frmRgt . evl e . (: r) . (,) x) 
 