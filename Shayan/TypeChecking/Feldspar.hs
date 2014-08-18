module TypeChecking.Feldspar () where

import MyPrelude

import Expression.Feldspar.GADTTyped
import qualified Type.Herbrand as TH

import Environment.Scoped  as ES

import TypeChecking
import InferenceMonad

instance Chk (Exp n) where
  type Cns (Exp n) = TH.EnvFld '[]
  type Env (Exp n) = ES.Env n
  chk ee r = case ee of
    ConI _         -> return TH.Int
    ConB _         -> return TH.Bol
    ConF _         -> return TH.Flt
    Var x          -> return (get x r)
    Abs eb         -> do ta <- newMT
                         tb <- chk eb (Ext ta r)
                         return (TH.Arr ta tb)
    App t  ef ea   -> do tf  <- chk ef r
                         ta  <- chk ea r
                         tfb <- newMT
                         addC (tf TH.:~: TH.Arr t tfb)
                         addC (t  TH.:~: ta)
                         return tfb
    Cnd ec et ef   -> do tc <- chk ec r
                         tt <- chk et r
                         tf <- chk ef r
                         addC (tc TH.:~: TH.Bol)
                         addC (tt TH.:~: tf)
                         return tt
    Whl ec eb ei   -> do t  <- newMT
                         tc <- chk ec (Ext t r)
                         tb <- chk eb (Ext t r)
                         ti <- chk ei r
                         addC (tc TH.:~: TH.Bol)
                         addC (tb TH.:~: t)
                         addC (ti TH.:~: t)
                         return t
    Tpl ef es      -> TH.Tpl <$> chk ef r <*> chk es r
    Fst ts e       -> do t   <- chk e r
                         tf  <- newMT
                         addC (t  TH.:~: TH.Tpl tf ts)
                         return tf
    Snd tf e       -> do t   <- chk e r
                         ts  <- newMT
                         addC (t  TH.:~: TH.Tpl tf ts)
                         return ts
    Ary el ef      -> do tl  <- chk el r
                         tf  <- chk ef (Ext TH.Int r)
                         addC (tl TH.:~: TH.Int)
                         return (TH.Ary tf)
    Len ta e       -> do t   <- chk e r
                         addC (t TH.:~: TH.Ary ta)
                         return TH.Int
    Ind ea ei      -> do ta  <- chk ea r
                         ti  <- chk ei r
                         taa <- newMT
                         addC (ta TH.:~: TH.Ary taa)
                         addC (ti TH.:~: TH.Int)
                         return taa
    Let t  el eb   -> do tl  <- chk el r
                         tb  <- chk eb (Ext t r)
                         addC (t TH.:~: tl)
                         return tb
    Cmx er ei      -> do tr  <- chk er r
                         ti  <- chk ei r
                         addC (tr TH.:~: TH.Flt)
                         addC (ti TH.:~: TH.Flt)
                         return TH.Cmx