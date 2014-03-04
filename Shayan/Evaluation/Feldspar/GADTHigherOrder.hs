module Evaluation.Feldspar.GADTHigherOrder where

import Prelude hiding (sin)
import Evaluation 
import Evaluation.Variable ()
import Expression.Feldspar.GADTHigherOrder hiding (fst,snd)
import qualified Expression.Feldspar.GADTValue as V
import Data.Array
import Data.Maybe(fromJust)
import Singleton 
import qualified Singleton.TypeFeldspar as G  
import Singleton.TypeFeldspar ()
import Singleton.Environment(get)
import qualified Type.Feldspar as A

type instance Val (Exp r t) = Trm t
type instance Env (Exp r t) = Trm r
type instance Val (G.Typ t , Exp r t) = Trm t
type instance Env (G.Typ t , Exp r t) = Trm r
type instance Val (Exp r ta -> Exp r tb) = Trm ta -> Trm tb
type instance Env (Exp r ta -> Exp r tb) = Trm r
type instance Val (G.Typ (A.Arr ta tb) , Exp r ta -> Exp r tb) = Trm ta -> Trm tb
type instance Env (G.Typ (A.Arr ta tb) , Exp r ta -> Exp r tb) = Trm r

instance HasSin G.Typ t => Evl (Exp r t) where
  evl e r = evl (sin :: G.Typ t , e) r

instance Evl (G.Typ t , Exp r t) where
  evl (t , egfo) r = case egfo of 
    ConI i       -> V.conI <$> pure i
    ConB b       -> V.conB <$> pure b
    Var x        -> return (get x r)
    App ta ef ea -> V.app  <$> evl (G.Arr ta t , ef) r <*> evl (ta , ea) r
    Abs eb       -> evl (t , eb) r
    Cnd ec et ef -> V.cnd  <$> evl (G.Bol , ec) r <*> evl (t , et) r 
                    <*> evl (t , ef) r
    Tpl ef es    -> case t of 
     G.Tpl tf ts -> V.tpl  <$> evl (tf , ef) r <*> evl (ts , es) r
     _           -> fail "Impossible!"
    Fst ts e     -> V.fst  <$> evl (G.Tpl t ts , e) r
    Snd tf e     -> V.snd  <$> evl (G.Tpl tf t , e) r                      
    Ary el ef    -> case t of 
      G.Ary ta   -> V.ary  <$> evl (G.Int , el) r <*> evl (G.Arr G.Int ta , ef) r
      _          -> fail "Impossible!"
    Len ta e     -> V.len  <$> evl (G.Ary ta , e)  r                        
    Ind ea ei    -> V.ind  <$> evl (G.Ary t , ea) r <*> evl (G.Int , ei) r 
    Whl ec eb ei -> V.whl  <$> evl (G.Arr t G.Bol , ec) r 
                    <*> evl (G.Arr t t , eb) r <*> evl (t , ei) r
    Let tl el eb -> evl (t , (App tl (Abs eb) el)) r
 
instance HasSin G.Typ (A.Arr ta tb) => Evl (Exp r ta -> Exp r tb) where
  evl e r = evl (sin :: G.Typ (A.Arr ta tb) , e) r
  
instance Evl (G.Typ (A.Arr ta tb) , Exp r ta -> Exp r tb) where 
  evl (G.Arr ta tb , f) r = pure (frmRgt . (\ e -> evl (tb , e) r) 
                                  . f . revEvl ta r)

revEvl :: G.Typ t -> Trm r -> Trm t -> Exp r t
revEvl t r v = case t of
  G.Int       -> ConI v
  G.Bol       -> ConB v
  G.Arr ta tb -> Abs (revEvl tb r . v . frmRgt . (\e -> evl (ta , e) r))
  G.Tpl tf ts -> Tpl (revEvl tf r (fst v)) (revEvl ts r (snd v)) 
  G.Ary ta    -> let (0,l) = bounds v in
    Ary (ConI l) (revEvl ta r . (fromJust . flip lookup (assocs v)) 
                  . frmRgt . (\e -> evl ( G.Int, e) r))
     
 