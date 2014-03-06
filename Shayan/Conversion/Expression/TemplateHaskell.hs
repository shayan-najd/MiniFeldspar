{-# LANGUAGE TemplateHaskell #-}
module Conversion.Expression.TemplateHaskell where
import qualified Language.Haskell.TH.Syntax             as TH

import qualified Expression.STLC.ADTUntypedNamed        as SAUP
import qualified Expression.STLC.ADTUntypedDebruijn     as SAUM
import qualified Expression.STLC.GADTFirstOrder         as SGFO
import qualified Type.STLC                              as SAS
import qualified Singleton.TypeSTLC                     as SG
import Conversion.Expression.STLC ()
import Singleton.TypeSTLC         () 
 
import qualified Environment.ADTTable                   as AT
import qualified Singleton.Environment                  as G

import Conversion   
import Singleton
import Existential

import Prelude hiding (filter)

type ExsSExp = Exs2 SGFO.Exp (G.Env SG.Typ) SG.Typ

instance TH.Quasi ErrM where
  qNewName          = return . TH.mkName 
  qReport b         = fail . if b 
                             then ("Error: " ++)
                             else ("Warning: " ++)     
  qRecover          = fail "Not Allowed!"
  qReify            = fail "Not Allowed!"
  qLookupName       = fail "Not Allowed!"
  qReifyInstances   = fail "Not Allowed!"
  qReifyRoles       = fail "Not Allowed!"
  qReifyAnnotations = fail "Not Allowed!"
  qReifyModule      = fail "Not Allowed!"
  qAddDependentFile = fail "Not Allowed!"
  qAddModFinalizer  = fail "Not Allowed!"
  qAddTopDecls      = fail "Not Allowed!"
  qLocation         = fail "Not Allowed!"
  qRunIO            = fail "Not Allowed!"
  qPutQ             = fail "Not Allowed!"
  qGetQ             = fail "Not Allowed!"
 
instance Cnv TH.Exp (SAUP.Exp TH.Name) where
  cnv (TH.ParensE e)                = cnv e
  cnv (TH.LitE (TH.IntegerL i))     = return (SAUP.Con (fromInteger i))
  cnv (TH.VarE n)                   = return (SAUP.Var n)
  cnv (TH.LamE (TH.VarP x : []) eb) = SAUP.Abs x <$> cnv eb  
                                         
  cnv (TH.AppE ef ea)               = SAUP.App <$> cnv ef <*> cnv ea  
  cnv (TH.InfixE (Just el) (TH.VarE (TH.Name (TH.OccName "+") 
         (TH.NameG TH.VarName (TH.PkgName "base") 
          (TH.ModName "GHC.Num")))) (Just er))                   
                                    = SAUP.Add <$> cnv el <*> cnv er
  cnv _                             = fail "Syntax Error!"
 
instance (HasSin SG.Typ (RevTrm t) , HasSin (G.Env SG.Typ) r , RevTrm t ~ t') => 
         Cnv (TH.Q (TH.TExp t) 
             , AT.Env TH.Name SAS.Typ
             , AT.Env TH.Name SAUM.Exp) (SGFO.Exp r t') where
          cnv (e , rt, rb) = do e'  :: TH.Exp <- (TH.runQ . TH.unTypeQ) e
                                e'' :: SAUP.Exp TH.Name <- cnv e'
                                -- normalization here
                                cnv (e'' , rt ,rb)            
      
norm :: a -> ErrM a
norm = return 