module Evaluation.STLC.GADTHigherOrder where

import Evaluation 
import Evaluation.Variable ()
import Expression.STLC.GADTHigherOrder
import Singleton.TypeSTLC
import qualified Expression.STLC.GADTValue as V
import Singleton
import Singleton.Environment hiding (Env)

type instance Val (Exp r t) = Trm t
type instance Env (Exp r t) = Trm r 

instance Evl (Exp r t) where
  evl egho r = case egho of
    Con i     -> V.con <$> pure i
    Var x     -> return (get x r)  
    Abs t  eb -> V.abs <$> pure (flip evl r . eb . cnvValExp t r)
    App ef ea -> V.app <$> evl' ef <*> evl' ea
    Add el er -> V.add <$> evl' el <*> evl' er
    where evl' :: (Evl e , Env e ~ Trm r) => e -> ErrM (Val e)
          evl' e = evl e r 

cnvValExp :: Typ t -> Trm r -> Trm t -> Exp r t
cnvValExp Int         _ i = Con i
cnvValExp (Arr ta tb) r f = Abs ta (cnvValExp tb r . f . frmRgt . flip evl r)
                             