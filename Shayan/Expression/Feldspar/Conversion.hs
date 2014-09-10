module Expression.Feldspar.Conversion () where

import MyPrelude

import qualified Language.Haskell.TH.Syntax              as TH
import qualified Expression.Feldspar.ADTUntypedNamed     as FAUN
import qualified Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Expression.Feldspar.GADTTyped           as FGTD
import qualified Expression.Feldspar.GADTFirstOrder      as FGFO
import qualified Expression.Feldspar.GADTHigherOrder     as FGHO
import qualified Expression.Feldspar.MiniWellScoped      as FMWS

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import qualified Environment.Plain  as EP
import qualified Environment.Scoped as ES
import qualified Environment.Typed  as ET

import Conversion
import Variable.Conversion                              ()
import Environment.Conversion                           ()
import Type.Feldspar.Conversion                         ()
import Expression.TemplateHaskell.Conversion            ()
import Expression.Feldspar.Conversions.Unquoting        ()
import Expression.Feldspar.Conversions.NameResolution   ()
import Expression.Feldspar.Conversions.ScopeWithnessing ()
import Expression.Feldspar.Conversions.TypeInference    ()
import Expression.Feldspar.Conversions.TypeWithnessing  ()
import Expression.Feldspar.Conversions.Lifting          ()
import Expression.Feldspar.Conversions.Normalization    ()

import Normalization
import Normalization.Feldspar.GADTHigherOrder (eta)

import Singleton

---------------------------------------------------------------------------------
-- Conversion from TH.TExp
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t' , t ~ RevTrm tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGHO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t' , t ~ RevTrm tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t', t ~ RevTrm tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ (RevTrm tt))  =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , v) = do e' :: FGUD.Exp n <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ (RevTrm tt)) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGUD.Exp n)
         where
  cnv (e , r , v) = do e' :: FAUD.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ (RevTrm tt)) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , r , v) = do e' :: FAUN.Exp TH.Name <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ (RevTrm tt)) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , r , v) = do e' :: TH.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ (RevTrm tt)) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             TH.Exp
         where
  cnv (e , _ , _) = TH.runQ (TH.unTypeQ e)

instance (n ~ Len r , HasSin TFG.Typ (RevTrm tt) , tt ~ tt') =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (TH.Q (TH.TExp tt'))
         where
  cnv (e , _ , _ ) = pure e

---------------------------------------------------------------------------------
-- Conversion from TH.Exp
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGHO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r)  =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , v) = do e' :: FGUD.Exp n <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGUD.Exp n)
         where
  cnv (e , r , v) = do e' :: FAUD.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , r , v) = do e' :: FAUN.Exp TH.Name <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , _ , _) = cnv (e , ())

instance (n ~ Len r) =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             TH.Exp
         where
  cnv (e , _ , _) = pure e

---------------------------------------------------------------------------------
-- Conversion from FAUN
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGHO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r)  =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , v) = do e' :: FGUD.Exp n <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGUD.Exp n)
         where
  cnv (e , r , v) = do e' :: FAUD.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , _ , vs) = do vs' :: EP.Env TH.Name <- cnv (vs , ())
                        cnv (e , vs')

instance (n ~ Len r) =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , _ , _ ) = pure e

---------------------------------------------------------------------------------
-- Conversion from FAUD
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGHO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , v) = do e' :: FGUD.Exp n <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGUD.Exp n)
         where
  cnv (e , r , _) = cnv (e , ET.len r)

instance (n ~ Len r) =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , _ , _ ) = pure e

---------------------------------------------------------------------------------
-- Conversion from FGUD
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGUD.Exp n , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGHO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGUD.Exp n , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGUD.Exp n , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ n' , n ~ Len r) =>
         Cnv (FGUD.Exp n , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n' TFA.Typ)
         where
  cnv (e , r , _) = do r' :: ES.Env n TFA.Typ <- cnv (r , ())
                       cnv (e , r')

instance (n ~ n' , n ~ Len r) =>
         Cnv (FGUD.Exp n , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGUD.Exp n')
         where
  cnv (e , _ , _) = pure e

---------------------------------------------------------------------------------
-- Conversion from FGTD
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGTD.Exp n TFA.Typ , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGHO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGTD.Exp n TFA.Typ , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGTD.Exp n TFA.Typ , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , _) = cnv (e , r)

instance (n ~ n' , r ~ r' , n ~ Len r) =>
         Cnv (FGTD.Exp n TFA.Typ , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n' TFA.Typ)
         where
  cnv (e , _ , _) = return e

---------------------------------------------------------------------------------
-- Conversion from FGFO
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGFO.Exp r t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGHO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGFO.Exp r t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , _) = cnv (e , r)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGFO.Exp r  t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , _ , _) = pure e

---------------------------------------------------------------------------------
-- Conversion from FGHO
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGHO.Exp r t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , _ , _) = cnv (nrm (eta e) , ())

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGHO.Exp r  t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , _ , _) = pure e

---------------------------------------------------------------------------------
-- Conversion from FMWS
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FMWS.Exp r  t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FMWS.Exp r' t')
         where
  cnv (e , _ , _) = pure e
