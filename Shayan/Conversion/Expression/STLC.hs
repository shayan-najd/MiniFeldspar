module Conversion.Expression.STLC where

import Prelude hiding (sin)

import qualified Expression.STLC.ADTUntypedNamed  as SAUP
import qualified Expression.STLC.ADTUntypedDebruijn  as SAUM
import qualified Expression.STLC.ADTChurch   as SACP
import qualified Expression.STLC.GADTFirstOrder         as SGFO
import qualified Expression.STLC.GADTHigherOrder        as SGHO

import qualified Type.STLC           as SAS
import qualified Singleton.TypeSTLC            as G
 
import qualified Environment.ADT          as A
import qualified Environment.ADTTable     as AT
import qualified Singleton.Environment    as G

import Conversion
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Existential ()
import Conversion.Expression.STLC.NameResolution ()
import Conversion.Expression.STLC.Lifting ()
import Conversion.Expression.STLC.TypeWithnessing ()
import Conversion.Expression.STLC.TypeInference ()
 
import TypeChecking.STLC   ()
 
import Existential
import Singleton
import Singleton.TypeSTLC ()

type ExsExp = Exs2 SGFO.Exp (G.Env G.Typ) G.Typ
type ExsTyp = ExsSin G.Typ
type SinTyp = HasSin G.Typ
type SinEnv = HasSin (G.Env G.Typ)

---------------------------------------------------------------------------------
-- Conversion from SAUP.Exp
---------------------------------------------------------------------------------
instance (Eq x , SinEnv r , SinTyp t') => 
         Cnv (SAUP.Exp x , AT.Env x SAS.Typ , AT.Env x SAUM.Exp) 
         (SGHO.Exp r t') where
  cnv (e , rt , rf) = do e' :: SGFO.Exp r t' <- cnv (e , rt , rf)         
                         cnv e'

instance (Eq x , SinEnv r , SinTyp t') =>  
         Cnv (SAUP.Exp x , AT.Env x SAS.Typ , AT.Env x SAUM.Exp) 
         (SGFO.Exp r t') where
  cnv (e , rt , rf) = do e' :: SAUM.Exp <- cnv (e , rt , rf)
                         cnv (e', map snd rt)
                                          
instance Eq x => 
         Cnv (SAUP.Exp x , AT.Env x SAS.Typ , AT.Env x SAUM.Exp) ExsExp  where
  cnv (e , rt , rf) = do e' :: SACP.Exp SAS.Typ <- cnv (e  , rt , rf)
                         cnv (e' , map snd rt)                    

instance Eq x => 
         Cnv (SAUP.Exp x , AT.Env x SAS.Typ , AT.Env x SAUM.Exp) 
         (SACP.Exp SAS.Typ) where
  cnv (e , rt , rf) = do e' :: SAUM.Exp         <- cnv (e , rt , rf)
                         cnv (e'  , map snd rt)

---------------------------------------------------------------------------------
-- Conversion from SAUM.Exp
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t') => 
         Cnv (SAUM.Exp , A.Env SAS.Typ) (SGHO.Exp r t') where
  cnv (e , r) = do e' :: SGFO.Exp r t' <- cnv (e , r)         
                   cnv e'

instance (SinEnv r , SinTyp t') => 
         Cnv (SAUM.Exp , A.Env SAS.Typ) (SGFO.Exp r t') where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)
                   cnv e'               
    
instance Cnv (SAUM.Exp , A.Env SAS.Typ) ExsExp where  
  cnv (e , r) = do e' :: SACP.Exp SAS.Typ <- cnv (e , r)
                   cnv (e' , r)
---------------------------------------------------------------------------------
-- Conversion from SACP.Exp
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t') => 
         Cnv (SACP.Exp SAS.Typ , A.Env SAS.Typ) (SGHO.Exp r t') where
  cnv (e , r) = do e' :: SGFO.Exp r t' <- cnv (e , r)         
                   cnv e'

instance (SinEnv r , SinTyp t') => 
         Cnv (SACP.Exp SAS.Typ , A.Env SAS.Typ) (SGFO.Exp r t') where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)       
                   cnv e'