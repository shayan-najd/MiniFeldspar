module Conversion.Expression.Feldspar.NameResolution () where

import MyPrelude

import qualified Expression.Feldspar.ADTUntypedNamed    as FAUN
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUD

import qualified Environment.Map                        as EM
import qualified Environment.Plain                      as EP

import Variable.Plain

import Conversion
import Conversion.Variable    ()

instance Eq x =>
         Cnv (FAUN.Exp x , EP.Env x) FAUD.Exp where
  cnv (e , r) = cnv (e , zip r [Zro ..])

instance Eq x =>
         Cnv (FAUN.Exp x , EM.Env x Var) FAUD.Exp where
  cnv (eaup , r) = let ?r = r in case eaup of
    FAUN.ConI i              -> FAUD.ConI <$@> i
    FAUN.ConB b              -> FAUD.ConB <$@> b
    FAUN.ConF b              -> FAUD.ConF <$@> b
    FAUN.Var  x              -> FAUD.Var  <$@> x
    FAUN.Abs  xb eb          -> FAUD.Abs  <$@> (xb , eb)
    FAUN.App  ef ea          -> FAUD.App  <$@> ef <*@> ea
    FAUN.Cnd  ec et ef       -> FAUD.Cnd  <$@> ec <*@> et <*@> ef
    FAUN.Whl  xc ec xb eb ei -> FAUD.Whl  <$@> (xc , ec)
                                          <*@> (xb , eb)  <*@> ei
    FAUN.Tpl  ef es          -> FAUD.Tpl  <$@> ef <*@> es
    FAUN.Fst  e              -> FAUD.Fst  <$@> e
    FAUN.Snd  e              -> FAUD.Snd  <$@> e
    FAUN.Ary  el xf ef       -> FAUD.Ary  <$@> el <*@> (xf , ef)
    FAUN.Len  e              -> FAUD.Len  <$@> e
    FAUN.Ind  ea ei          -> FAUD.Ind  <$@> ea <*@> ei
    FAUN.Let  xl el eb       -> FAUD.Let  <$@> el <*@> (xl , eb)
    FAUN.Cmx  er ei          -> FAUD.Cmx  <$@> er <*@> ei

instance Eq x =>
         Cnv ((x , FAUN.Exp x) , EM.Env x Var)
         FAUD.Exp where
  cnv ((x , e) , r) = cnv (e , (x , Zro) : fmap (fmap Suc) r)

instance (x ~ x') =>
         Cnv (FAUD.Exp , (EP.Env x , EM.Env Var x)) (FAUN.Exp x') where
  cnv (eaup , r) = let ?r = r in case eaup of
    FAUD.ConI i        -> FAUN.ConI <$@> i
    FAUD.ConB b        -> FAUN.ConB <$@> b
    FAUD.ConF b        -> FAUN.ConF <$@> b
    FAUD.Var  x        -> let ?r = snd r in FAUN.Var  <$@> x
    FAUD.Abs  eb       -> do (xb , eb') <- cnvf eb
                             pure (FAUN.Abs xb eb')
    FAUD.App  ef ea    -> FAUN.App  <$@> ef <*@> ea
    FAUD.Cnd  ec et ef -> FAUN.Cnd  <$@> ec <*@> et <*@> ef
    FAUD.Whl  ec eb ei -> do (xc , ec') <- cnvf ec
                             (xb , eb') <- cnvf eb
                             FAUN.Whl xc ec' xb eb' <$@> ei
    FAUD.Tpl  ef es    -> FAUN.Tpl  <$@> ef <*@> es
    FAUD.Fst  e        -> FAUN.Fst  <$@> e
    FAUD.Snd  e        -> FAUN.Snd  <$@> e
    FAUD.Ary  el ef    -> do (xf , ef') <- cnvf ef
                             FAUN.Ary  <$@> el <*> pure xf <*> pure ef'
    FAUD.Len  e        -> FAUN.Len  <$@> e
    FAUD.Ind  ea ei    -> FAUN.Ind  <$@> ea <*@> ei
    FAUD.Let  el eb    -> do (xl , eb') <- cnvf eb
                             FAUN.Let  <$> pure xl <*@> el <*> pure eb'
    FAUD.Cmx  er ei    -> FAUN.Cmx  <$@> er <*@> ei
    where
      cnvf :: FAUD.Exp -> ErrM (x , FAUN.Exp x)
      cnvf e = case r of
        (x : xs , r') -> do e' <- cnv (e ,
                                      (xs , (Zro , x) :
                                            fmap (\(v , n) -> (Suc v , n)) r'))
                            pure (x , e')
        _            -> fail "Bad Name Pool!"