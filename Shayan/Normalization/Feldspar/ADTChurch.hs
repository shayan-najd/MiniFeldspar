{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor, ScopedTypeVariables,GADTs, ImplicitParams #-}
module Normalization.Feldspar.ADTChurch where

import Expression.Feldspar.ADTChurch
import Control.Applicative.Recursion
import Variable.ADT
import ChangeMonad

nrm :: Exp t-> Exp t
nrm = tilNotChg nrmOne
    
nrmOne :: Exp t -> Chg (Exp t)
nrmOne ee = case ee of
  ConI i              -> pure (ConI i)
  ConB b              -> pure (ConB b)
  Var x               -> pure (Var  x)
  App (Abs _ eb) ea   -> chg  (sbs eb Zro ea)
  App ef         ea   -> App <$@> ef <*@> ea    
  Abs _ (App ef (Var Zro))
    | notElem Zro (fre ef) -> chg ef
  Abs t eb            -> Abs <$> pure t  <*@> eb
  Cnd (ConB b) et ef  -> chg (if b then et else ef)
  Cnd ec et ef        -> Cnd <$@> ec <*@> et <*@> ef
  Tpl ef es           -> Tpl <$@> ef <*@> es
  Fst (Tpl ef _)      -> chg ef
  Fst e               -> Fst <$@> e
  Snd (Tpl _ es)      -> chg es
  Snd e               -> Snd <$@> e                       
  Ary el ef           -> Ary <$@> el <*@> ef
  Len (Ary el _ )     -> chg el                         
  Len e               -> Len <$@> e                         
  Ind (Ary _  ef) ei  -> chg (App ef ei)                         
  Ind ea ei           -> Ind <$@> ea <*@> ei                         
  Whl ec eb ei        -> Whl <$@> ec <*@> eb <*@> ei
  Let el eb           -> Let <$@> el <*@> eb
  where 
    ?cnv = nrmOne