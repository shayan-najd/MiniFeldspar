module Conversion.Expression.Feldspar.Lifting where

import Prelude hiding (sin)
import qualified Expression.Feldspar.GADTFirstOrder  as FGFO
import qualified Expression.Feldspar.GADTHigherOrder as FGHO 

import qualified Singleton.TypeFeldspar as FG
import qualified Singleton.Environment  as G

import Conversion hiding ((<$@>),(<*@>),pure)
import Conversion.Type.Feldspar ()
import Conversion.Variable      ()
import Conversion.Existential   ()
 
pur :: a -> ErrM a
pur = return

instance (t ~ t' , r ~ r') => 
         Cnv (FGFO.Exp r t , G.Env FG.Typ r) (FGHO.Exp r' t') where
  cnv (e , r) = cnv (e , ((G.map FGHO.Var . G.cnvGEnvtoGVar) r))

instance (t ~ t' , r ~ r') => 
         Cnv (FGFO.Exp r t , G.Env (FGHO.Exp r) r) (FGHO.Exp r' t') where
  cnv = uncurry cnvv
   where
   cnvv :: forall rr tt.
                 FGFO.Exp rr tt -> G.Env (FGHO.Exp rr) rr 
                 -> ErrM (FGHO.Exp rr tt)
   cnvv egdt r = case egdt of  
      FGFO.ConI i       -> FGHO.ConI <$> pur i
      FGFO.ConB b       -> FGHO.ConB <$> pur b
      FGFO.Var v        -> pur (G.gets v r)
      FGFO.Abs eb       -> FGHO.Abs <$@> eb
      FGFO.App ta ef ea -> FGHO.App <$> pur ta <*@> ef <*@> ea
      FGFO.Cnd ec et ef -> FGHO.Cnd <$@> ec <*@> et <*@> ef
      FGFO.Whl ec eb ei -> FGHO.Whl <$@> ec <*@> eb <*@> ei
      FGFO.Tpl ef es    -> FGHO.Tpl <$@> ef <*@> es
      FGFO.Ary el ef    -> FGHO.Ary <$@> el <*@> ef
      FGFO.Ind ea ei    -> FGHO.Ind <$@> ea <*@> ei
      FGFO.Fst ts e     -> FGHO.Fst <$> pur ts <*@> e
      FGFO.Snd tf e     -> FGHO.Snd <$> pur tf <*@> e
      FGFO.Len ta e     -> FGHO.Len <$> pur ta <*@> e 
      FGFO.Let tl el eb -> FGHO.Let <$> pur tl <*@> el <*@> eb 
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