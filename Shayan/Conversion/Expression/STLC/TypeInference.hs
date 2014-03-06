module Conversion.Expression.STLC.TypeInference where

import qualified Expression.STLC.ADTUntypedDebruijn  as SAUM
import qualified Expression.STLC.ADTChurch   as SACP
 
import qualified Type.STLC as SAS
import qualified Type.Herbrand as H

import qualified Environment.ADT          as A
 
import Conversion
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Existential ()
 
import TypeChecking.STLC   ()

import Inference
 
import Data.Traversable (traverse)

instance Cnv (SAUM.Exp, A.Env SAS.Typ) (SACP.Exp SAS.Typ) where
  cnv (e , r) = do e' :: SACP.Exp () <- cnv e
                   cnv (e' , r)

instance Cnv SAUM.Exp (SACP.Exp ()) where
  cnv eaum = case eaum of
       SAUM.Con i     -> SACP.Con <$> pure i
       SAUM.Var v     -> SACP.Var <$> pure v
       SAUM.Abs eb    -> SACP.Abs <$> pure () <*@> eb
       SAUM.App ef ea -> SACP.App <$@> ef <*@> ea
       SAUM.Add el er -> SACP.Add <$@> el <*@> er 
       where
         ?cnv = cnv

instance Cnv (SACP.Exp (), A.Env SAS.Typ) (SACP.Exp SAS.Typ) where
  cnv (e , r) = do r' :: A.Env (H.Typ (H.EnvIntArr '[])) <- cnv r
                   e' <- typInf e r'                    
                   traverse cnv e'