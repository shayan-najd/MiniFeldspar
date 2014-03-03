module Conversion.Expression.Feldspar () where

import Prelude hiding (sin)
import qualified Expression.Feldspar.ADTUntypedNamed    as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Expression.Feldspar.ADTChurch          as FACP
import qualified Expression.Feldspar.GADTFirstOrder     as FGFO

import qualified Type.Feldspar           as FAS
import qualified Singleton.TypeFeldspar  as FG

import qualified Environment.ADT         as A
import qualified Environment.ADTTable    as AT
import qualified Singleton.Environment   as G

import Conversion
import Conversion.Type.Feldspar                       ()
import Conversion.Variable                            ()
import Conversion.Existential                         ()
import Conversion.Expression.Feldspar.NameResolution  ()
import Conversion.Expression.Feldspar.TypeInference   ()
import Conversion.Expression.Feldspar.TypeWithnessing ()
import Conversion.Expression.Feldspar.Lifting         ()
 
import TypeChecking.Feldspar.ADTChurch ()

import Existential
import Singleton

type ExsExp = Exs2 FGFO.Exp (G.Env FG.Typ) FG.Typ
type SinTyp = HasSin FG.Typ
type SinEnv = HasSin (G.Env FG.Typ)
  
---------------------------------------------------------------------------------
-- Conversion from FAUP
---------------------------------------------------------------------------------
instance (Eq x , SinEnv r , SinTyp t') => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUM.Exp) 
             (FGFO.Exp r t')  where
  cnv (e , rt , rf) = do e' :: FAUM.Exp <- cnv (e , rt , rf)
                         cnv (e', map snd rt)

instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUM.Exp) ExsExp  where
  cnv (e , rt , rf) = do e' :: FACP.Exp FAS.Typ <- cnv (e  , rt , rf)
                         cnv (e' , map snd rt)                   

instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUM.Exp) 
             (FACP.Exp FAS.Typ) where
  cnv (e , rt , rf) = do e' :: FAUM.Exp <- cnv (e , rt , rf)
                         cnv (e'  , map snd rt)                          
---------------------------------------------------------------------------------
-- Conversion from FAUM
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t') => 
         Cnv (FAUM.Exp , A.Env FAS.Typ) (FGFO.Exp r t')  where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)       
                   cnv e'

instance Cnv (FAUM.Exp, A.Env FAS.Typ) ExsExp where
  cnv (e , r) = do e' :: FACP.Exp FAS.Typ <- cnv (e , r)
                   cnv (e' , r)
---------------------------------------------------------------------------------
-- Conversion from FACP
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t') => 
         Cnv (FACP.Exp FAS.Typ, A.Env FAS.Typ) (FGFO.Exp r t')  where
  cnv (e , r) = do e' :: ExsExp           <- cnv (e , r)       
                   cnv e' 
 