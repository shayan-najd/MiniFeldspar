module Conversion.Expression.Feldspar.NameResolution () where

import Prelude ()
import MyPrelude 

import qualified Expression.Feldspar.ADTUntypedNamed    as FAUN
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUD

import qualified Environment.Map                        as EM
import qualified Environment.Plain                      as EP

import Variable.Plain 

import Conversion
import Conversion.Nat         () 
import Conversion.Environment ()
import Conversion.Variable    ()

instance Eq x => 
         Cnv (FAUN.Exp x , EP.Env x) FAUD.Exp where
  cnv (e , r) = cnv (e , zip r [Zro ..])
     
instance Eq x => 
         Cnv (FAUN.Exp x , EM.Env x Var) FAUD.Exp where
  cnv (eaup , r) = let ?r = r in case eaup of
    FAUN.ConI i              -> FAUD.ConI <$@> i
    FAUN.ConB b              -> FAUD.ConB <$@> b
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
    FAUN.Ind  ea ei          -> FAUD.Ind  <$@> ea <*@> ei 
    FAUN.Len  e              -> FAUD.Len  <$@> e
    FAUN.Let  xl el eb       -> FAUD.Let  <$@> el <*@> (xl , eb)
 
instance Eq x => 
         Cnv ((x , FAUN.Exp x) , EM.Env x Var) 
         FAUD.Exp where
  cnv ((x , e) , r) = cnv (e , (x , Zro) : fmap (fmap Suc) r)