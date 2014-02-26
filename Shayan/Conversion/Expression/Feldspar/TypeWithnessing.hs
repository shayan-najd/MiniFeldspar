{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs
           , ImplicitParams, PolyKinds #-}
module Conversion.Expression.Feldspar.TypeWithnessing where

import Prelude hiding (sin)
import qualified Expression.Feldspar.ADTChurch  as FACP
import qualified Expression.Feldspar.GADTFirstOrder        as FGFO

import qualified Singleton.TypeFeldspar            as FG
import qualified Type.Feldspar as FAS
 
import qualified Environment.ADT         as A
import qualified Singleton.Environment   as G

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Existential ()
import Conversion.Environment ()

import SingletonEquality
import SingletonEquality.EnvironmentGADT ()
import SingletonEquality.TypeFeldsparGADT ()

import Existential

type ExsTyp = ExsSin FG.Typ
type ExsExp = Exs2 FGFO.Exp (G.Env FG.Typ) FG.Typ
 
instance Cnv (FACP.Exp FAS.Typ , A.Env FAS.Typ) ExsExp where
  cnv (FACP.ConI i , r)       = do 
    ExsSin r' <- cnv r
    return (Exs2 (FGFO.ConI i) r' FG.Int)
  cnv (FACP.ConB i , r)       = do 
    ExsSin r' <- cnv r
    return (Exs2 (FGFO.ConB i) r' FG.Bol)
  cnv (FACP.Var x  , r)       = do 
    Exs2 x' r' t' <- cnv (x , r)
    return (Exs2 (FGFO.Var x') r' t')
  cnv (FACP.Abs ta eb , r)    = do 
    ExsSin ta'                    :: ExsTyp <- cnv ta
    Exs2 eb' (ta'' `G.Ext` r') tb :: ExsExp <- cnv(eb , ta : r)
    Rfl <- eqlSin ta' ta''
    return (Exs2 (FGFO.Abs eb') r' (FG.Arr ta' tb))
  cnv (FACP.App ef ea , r)    = do 
    Exs2 ef' rf (FG.Arr ta tb) :: ExsExp <- cnv (ef , r)
    Exs2 ea' ra ta'            :: ExsExp <- cnv (ea , r)
    Rfl <- eqlSin rf ra
    Rfl <- eqlSin ta ta'
    return (Exs2 (FGFO.App ta ef' ea') rf tb)
  cnv (FACP.Cnd ec et ef , r) = do
    Exs2 ec' rc FG.Bol :: ExsExp <- cnv (ec , r)
    Exs2 et' rt tt      :: ExsExp <- cnv (et , r)
    Exs2 ef' rf tf      :: ExsExp <- cnv (ef , r)
    Rfl <- eqlSin rc rt
    Rfl <- eqlSin rc rf
    Rfl <- eqlSin tt tf
    return (Exs2 (FGFO.Cnd ec' et' ef') rc tt)
  cnv (FACP.Whl ec eb ei , r) = do 
    Exs2 ec' rc (FG.Arr tac FG.Bol) :: ExsExp <- cnv (ec , r)
    Exs2 eb' rb (FG.Arr tab tbb )    :: ExsExp <- cnv (eb , r)
    Exs2 ei' ri ti                    :: ExsExp <- cnv (ei , r)
    Rfl <- eqlSin rc rb
    Rfl <- eqlSin rc ri
    Rfl <- eqlSin tac tab
    Rfl <- eqlSin tac tbb
    Rfl <- eqlSin tac ti
    return (Exs2 (FGFO.Whl ec' eb' ei') rc tac)
  cnv (FACP.Tpl ef es , r)    = do 
    Exs2 ef' rf tf :: ExsExp <- cnv (ef , r)
    Exs2 es' rs ts :: ExsExp <- cnv (es , r)
    Rfl <- eqlSin rf rs
    Rfl <- eqlSin tf ts
    return (Exs2 (FGFO.Tpl ef' es') rf (FG.Tpl tf ts))
  cnv (FACP.Fst e , r)        = do 
    Exs2 e' r' (FG.Tpl tf ts) :: ExsExp <- cnv (e , r)
    return (Exs2 (FGFO.Fst ts e') r' tf)
  cnv (FACP.Snd e , r)        = do 
    Exs2 e' r' (FG.Tpl tf ts) :: ExsExp <- cnv (e , r)
    return (Exs2 (FGFO.Snd tf e') r' ts)  
  cnv (FACP.Ary el ef , r)    = do 
    Exs2 el' rl FG.Int             :: ExsExp <- cnv (el , r)
    Exs2 ef' rf (FG.Arr FG.Int ta) :: ExsExp <- cnv (ef , r)
    Rfl <- eqlSin rl rf
    return (Exs2 (FGFO.Ary el' ef') rl (FG.Ary ta))
  cnv (FACP.Len e , r)        = do 
    Exs2 e' r' (FG.Ary ta)  :: ExsExp <- cnv (e , r)
    return (Exs2 (FGFO.Len ta e') r' FG.Int)
  cnv (FACP.Ind ea ei , r)    = do 
    Exs2 ea' ra (FG.Ary ta) :: ExsExp <- cnv (ea , r)
    Exs2 ei' ri FG.Int      :: ExsExp <- cnv (ei , r)
    Rfl <- eqlSin ra ri
    return (Exs2 (FGFO.Ind ea' ei') ra ta)
  cnv (FACP.Let el eb , r)    = do 
    Exs2 el' rl tl               :: ExsExp  <- cnv (el , r)
    tl'                          :: FAS.Typ <- cnv tl
    Exs2 eb' (G.Ext  tl'' rb) tb :: ExsExp  <- cnv (eb , tl' : r)
    Rfl <- eqlSin tl tl''
    Rfl <- eqlSin rb rl
    return (Exs2 (FGFO.Let tl'' el' eb') rb tb)