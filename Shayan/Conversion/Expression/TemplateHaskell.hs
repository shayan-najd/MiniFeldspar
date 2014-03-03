{-# LANGUAGE TemplateHaskell #-}
module Conversion.Expression.TemplateHaskell where
import qualified Language.Haskell.TH.Syntax             as TH

import qualified Expression.STLC.ADTUntypedNamed        as SAUP
import qualified Expression.STLC.ADTUntypedDebruijn     as SAUM
import qualified Expression.STLC.GADTFirstOrder         as SGFO
import qualified Type.STLC                    as SAS
import qualified Singleton.TypeSTLC                     as SG
import Conversion.Expression.STLC ()
import Singleton.TypeSTLC () 

import qualified Expression.Feldspar.ADTUntypedNamed    as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Expression.Feldspar.GADTFirstOrder     as FGFO
import qualified Type.Feldspar                as FAS
import qualified Singleton.TypeFeldspar                 as FG
import Conversion.Expression.Feldspar ()
import Singleton.TypeFeldspar () 

import Expression.Feldspar.GADTValue (ind,len,arr)

import qualified Environment.ADTTable                  as AT
import qualified Singleton.Environment                 as G

import Conversion   
import Singleton
import Existential

import Prelude hiding (filter)

type ExsFExp = Exs2 FGFO.Exp (G.Env FG.Typ) FG.Typ 
type ExsSExp = Exs2 SGFO.Exp (G.Env SG.Typ) SG.Typ

instance TH.Quasi ErrM where
  qNewName    = return . TH.mkName 
  qReport     = undefined
  qRecover    = undefined
  qReify      = undefined
  qLookupName = undefined
  qReifyInstances   = undefined
  qReifyRoles       = undefined
  qReifyAnnotations = undefined
  qReifyModule      = undefined
  qAddDependentFile = undefined
  qAddModFinalizer  = undefined
  qAddTopDecls      = undefined
  qLocation   = undefined
  qRunIO      = undefined
  qPutQ       = undefined
  qGetQ       = undefined
 
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

instance Cnv TH.Exp (FAUP.Exp TH.Name) where
  cnv (TH.ParensE e)            = cnv e
  cnv (TH.LitE (TH.IntegerL i)) = return (FAUP.ConI (fromInteger i))
  cnv (TH.ConE n) | n == 'True  = return (FAUP.ConB True)
                  | n == 'False = return (FAUP.ConB False)
  cnv (TH.VarE n)               = return (FAUP.Var n)
  cnv (TH.LamE [TH.VarP x] eb)  = FAUP.Abs x <$> cnv eb 
  cnv (TH.InfixE (Just el) ef (Just er))
                                =  cnv (TH.AppE (TH.AppE ef el) er)
  cnv (TH.AppE (TH.VarE n) ea) 
                  | n == 'fst   = FAUP.Fst <$> cnv ea
                  | n == 'snd   = FAUP.Snd <$> cnv ea
                  | n == 'len   = FAUP.Len <$> cnv ea
  cnv (TH.AppE (TH.AppE (TH.VarE n) el) er)
                  | n == 'ind   = FAUP.Ind <$> cnv el <*> cnv er 
                  | n == 'arr   = FAUP.Ary <$> cnv el <*> cnv er 
  cnv (TH.AppE ef ea)           = FAUP.App <$> cnv ef <*> cnv ea  
  cnv (TH.CondE ec et ef)       = FAUP.Cnd <$> cnv ec <*> cnv et <*> cnv ef
  cnv (TH.TupE [ef , es])       = FAUP.Tpl <$> cnv ef <*> cnv es
  cnv (TH.LetE [TH.ValD (TH.VarP x) (TH.NormalB el) []] eb) 
                                = FAUP.Let x <$> cnv el <*> cnv eb
  cnv _                         = fail "Syntax Error!"
 
instance (HasSin SG.Typ (RevTrm t) , HasSin (G.Env SG.Typ) r , RevTrm t ~ t') => 
         Cnv (TH.Q (TH.TExp t) 
             , AT.Env TH.Name SAS.Typ
             , AT.Env TH.Name SAUM.Exp) (SGFO.Exp r t') where
          cnv (e , rt, rb) = do e'  :: TH.Exp <- (TH.runQ . TH.unTypeQ) e
                                e'' :: SAUP.Exp TH.Name <- cnv e'
                                -- normalization here
                                cnv (e'' , rt ,rb)            

instance (HasSin FG.Typ (RevTrm t) , HasSin (G.Env FG.Typ) r , RevTrm t ~ t') => 
         Cnv (TH.Q (TH.TExp t) 
             , AT.Env TH.Name FAS.Typ
             , AT.Env TH.Name FAUM.Exp) (FGFO.Exp r t') where
          cnv (e , rt, rb) = do e'  :: TH.Exp <- (TH.runQ . TH.unTypeQ) e
                                e'' :: FAUP.Exp TH.Name <- cnv e'
                                -- normalization here
                                cnv (e'' , rt ,rb)
 
      
norm :: a -> ErrM a
norm = return 