{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds, DataKinds, TypeOperators #-}
module Conversion.Expression.Feldspar.Lifting where

import Prelude hiding (sin)
import qualified Expression.Feldspar.GADTFirstOrder  as FGFO
import qualified Expression.Feldspar.GADTHigherOrder as FGHO 

import qualified Singleton.TypeFeldspar as FG
import qualified Singleton.Environment  as G

import Conversion hiding ((<$@>),(<*@>))
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Existential ()

import ErrorMonad
import Singleton
 
type SinTyp = HasSin FG.Typ
type SinEnv = HasSin (G.Env FG.Typ)

instance (t ~ t' , r ~ r' , SinEnv r) => 
         Cnv (FGFO.Exp r t) (FGHO.Exp r' t') where
  cnv = \ e -> cnv (e , ((G.map FGHO.Var . G.cnvGEnvtoGVar) 
                         (sin :: G.Env FG.Typ r)))
               
instance (t ~ t' , r ~ r') => 
         Cnv (FGFO.Exp r t , G.Env (FGHO.Exp r) r) (FGHO.Exp r' t') where
  cnv = uncurry cnvGToGHO
   where
   cnvGToGHO :: forall rr tt.
                 FGFO.Exp rr tt -> G.Env (FGHO.Exp rr) rr 
                 -> ErrM (FGHO.Exp rr tt)
   cnvGToGHO egdt r = case egdt of  
      FGFO.ConI i       -> FGHO.ConI <$> pure i
      FGFO.ConB b       -> FGHO.ConB <$> pure b
      FGFO.Var v        -> pure (G.gets v r)
      FGFO.Abs ta eb    -> FGHO.Abs ta <$@> eb
      FGFO.App ef ea    -> FGHO.App <$@> ef <*@> ea
      FGFO.Cnd ec et ef -> FGHO.Cnd <$@> ec <*@> et <*@> ef
      FGFO.Whl ec eb ei -> FGHO.Whl <$@> ec <*@> eb <*@> ei
      FGFO.Tpl ef es    -> FGHO.Tpl <$@> ef <*@> es
      FGFO.Ary el ef    -> FGHO.Ary <$@> el <*@> ef
      FGFO.Ind ea ei    -> FGHO.Ind <$@> ea <*@> ei
      FGFO.Fst e        -> FGHO.Fst <$@> e
      FGFO.Snd e        -> FGHO.Snd <$@> e
      FGFO.Len e        -> FGHO.Len <$@> e 
      FGFO.Let tl el eb -> FGHO.Let tl <$@> el <*@> eb 
      where     
        c :: Cnv (a , G.Env (FGHO.Exp rr) rr) b => a -> ErrM b
        c e = cnv (e , r)
        infixl 4 <$@>
        (<$@>) :: Cnv (a , G.Env (FGHO.Exp rr) rr) a' => (a' -> b) -> a -> ErrM b
        el <$@> er = el <$> c er
        infixl 4 <*@>
        (<*@>) :: Cnv (a , G.Env (FGHO.Exp rr) rr) a' =>  
                  ErrM (a' -> b) -> a -> ErrM b
        el <*@> er = el <*> c er      
  
instance (ta ~ ta' , tb ~ tb' , r ~ r') => 
         Cnv (FGFO.Exp (ta ': r) tb , G.Env (FGHO.Exp r) r) 
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb') where
   cnv (eb , r) = return (FGHO.prdAll . frmRgt . cnv' eb .
                          G.wkn FGHO.sucAll . flip G.Ext r) 
                  where
                    cnv' :: forall rr tt. FGFO.Exp rr tt  -> 
                            G.Env (FGHO.Exp rr) rr -> 
                            ErrM (FGHO.Exp rr tt)
                    cnv' = curry cnv