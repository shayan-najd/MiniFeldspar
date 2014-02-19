{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.STLC.NameResolution where

import Prelude hiding (sin)

import qualified Expression.STLC.ADTUntypedNamed    as SAUP
import qualified Expression.STLC.ADTUntypedDebruijn as SAUM
import qualified Type.STLC      as SAS
import qualified Variable.ADT             as A
import qualified Environment.ADTTable     as AT

import Conversion
 
instance Eq x => 
         Cnv (SAUP.Exp x , AT.Env x SAS.Typ , AT.Env x SAUM.Exp) SAUM.Exp where
  cnv = \ (e , rt , rf) -> cnvExpUUToU e (zip (map fst rt) [A.Zro ..]) rf
    where
      cnvExpUUToU eaup rb rf = case eaup of      
        SAUP.Con i     -> return (SAUM.Con i)
        SAUP.Var s     -> case (AT.get s rb , AT.get s rf) of
          (Just x  , _)      -> SAUM.Var <$> pure x
          (Nothing , Just e) -> pure e
          _                  -> fail "Scope Error!"
        SAUP.Abs s  eb -> SAUM.Abs <$> 
                          cnvExpUUToU eb ((s , A.Zro) : fmap A.Suc `map` rb) rf 
        SAUP.App ef ea -> SAUM.App <$@> ef <*@> ea
        SAUP.Add el er -> SAUM.Add <$@> el <*@> er
        where
          ?cnv = \e -> cnvExpUUToU e rb rf