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
    App t  ef ea   -> do ta <- chk ea r
                         tf <- chk ef r
                         tb <- newMT
                         addC (tf TH.:~: TH.Arr ta tb)
                         addC (t  TH.:~: ta)
                         return tb
    Cnd ec et ef   -> do tc <- chk ec r
                         addC (tc TH.:~: TH.Bol)
                         tt <- chk et r
                         tf <- chk ef r
                         addC (tt TH.:~: tf)
                         return tt
    Whl ec eb ei   -> do t  <- newMT
                         tc <- chk ec (Ext t r)
                         addC (tc TH.:~: TH.Bol)
                         tb <- chk eb (Ext t r)
                         addC (tb TH.:~: t)
                         ti <- chk ei r
                         addC (ti TH.:~: t)
                         return t
    Tpl ef es      -> TH.Tpl <$> chk ef r <*> chk es r
    Fst t e        -> do te  <- chk e r
                         tf  <- newMT
                         ts  <- newMT
                         addC (te TH.:~: TH.Tpl tf ts)
                         addC (t  TH.:~: ts)
                         return tf
    Snd t  e       -> do te  <- chk e r
                         tf  <- newMT
                         ts  <- newMT
                         addC (te TH.:~: TH.Tpl tf ts)
                         addC (t  TH.:~: tf)
                         return ts
    Ary el ef      -> do tl  <- chk el r
                         addC (tl TH.:~: TH.Int)
                         tf  <- chk ef (Ext TH.Int r)
                         return (TH.Ary tf)
    Len t e        -> do te  <- chk e r
                         ta  <- newMT
                         addC (te TH.:~: TH.Ary ta)
                         addC (t  TH.:~: ta)
                         return TH.Int
    Ind ea ei      -> do ta  <- chk ea r
                         taa <- newMT
                         addC (ta TH.:~: TH.Ary taa)
                         ti  <- chk ei r
                         addC (ti TH.:~: TH.Int)
                         return taa
    Let t  el eb   -> do tl  <- chk el r
                         tb  <- chk eb (Ext tl r)
                         addC (t TH.:~: tl)
                         return tb
    Cmx er ei      -> do tr  <- chk er r
                         addC (tr TH.:~: TH.Flt)
                         ti  <- chk ei r
                         addC (ti TH.:~: TH.Flt)
                         return TH.Cmx
    Non            -> do t <- newMT
                         return (TH.May t)
    Som e          -> do t   <- chk e r
                         return (TH.May t)
    May t em en es -> do mm  <- chk em r
                         a   <- newMT
                         addC (mm TH.:~: TH.May a)
                         bn  <- chk en r
                         bs  <- chk es (Ext a r)
                         addC (bn TH.:~: bs)
                         addC (t  TH.:~: a)
                         return bs