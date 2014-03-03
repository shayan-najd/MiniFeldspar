module Conversion.Expression.Feldspar.ScopeWithnessing where

import qualified Expression.Feldspar.ADTUntypedDebruijn  as FAUM
import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUM
 
import qualified Singleton.Nat as G

import Conversion
import Conversion.Nat           ()
import Conversion.Type.Feldspar ()
import Conversion.Variable      ()

import Existential

type ExsNat = ExsSin G.Nat 
type ExsExp = Exs1 FGUM.Exp G.Nat

instance Cnv FAUM.Exp ExsExp where
  cnv ee = do let n = FAUM.fre ee
              ExsSin n' :: ExsNat <- cnv n 
              e' <- cnvv ee n'
              return (Exs1 e' n')
   where
     cnvv :: FAUM.Exp -> G.Nat n -> ErrM (FGUM.Exp n)
     cnvv eaum n = case eaum of
       FAUM.ConI i       -> FGUM.ConI <$> pure i
       FAUM.ConB b       -> FGUM.ConB <$> pure b 
       FAUM.Var v        -> FGUM.Var  <$> cnv  (v , n)
       FAUM.Abs eb       -> FGUM.Abs  <$> cnvv eb (G.Suc n)
       FAUM.App ef ea    -> FGUM.App  <$@> ef <*@> ea
       FAUM.Cnd ec et ef -> FGUM.Cnd  <$@> ec <*@> et <*@> ef 
       FAUM.Whl ec eb ei -> FGUM.Whl  <$@> ec <*@> eb <*@> ei
       FAUM.Tpl ef es    -> FGUM.Tpl  <$@> ef <*@> es
       FAUM.Ary el ef    -> FGUM.Ary  <$@> el <*@> ef
       FAUM.Ind ea ei    -> FGUM.Ind  <$@> ea <*@> ei
       FAUM.Fst e        -> FGUM.Fst  <$@> e
       FAUM.Snd e        -> FGUM.Snd  <$@> e
       FAUM.Len e        -> FGUM.Len  <$@> e 
       FAUM.Let el eb    -> FGUM.Let  <$@> el <*> cnvv eb (G.Suc n) 
       where
           ?cnv = \ e -> cnvv e n
         