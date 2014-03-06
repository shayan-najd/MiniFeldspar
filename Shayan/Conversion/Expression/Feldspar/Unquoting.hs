{-# LANGUAGE TemplateHaskell #-}
module Conversion.Expression.Feldspar.Unquoting where

import Conversion
import qualified Expression.Feldspar.ADTUntypedNamed as FAUP
import qualified Language.Haskell.TH.Syntax          as TH
import Expression.Feldspar.GADTValue (ind,len,ary, whl)


instance Cnv TH.Exp (FAUP.Exp TH.Name) where
  cnv (TH.ParensE e)            = cnv e
  cnv (TH.LitE (TH.IntegerL i)) = return (FAUP.ConI (fromInteger i))
  cnv (TH.ConE n) | n == 'True  = return (FAUP.ConB True)
                  | n == 'False = return (FAUP.ConB False)
  cnv (TH.VarE n)               = return (FAUP.Var n)
  cnv (TH.LamE [TH.VarP x] eb)  = FAUP.Abs x <$> cnv eb 
  cnv (TH.InfixE (Just el) ef (Just er))
                                = cnv (TH.AppE (TH.AppE ef el) er)
  cnv (TH.AppE (TH.VarE n) ea) 
                  | n == 'fst   = FAUP.Fst <$> cnv ea
                  | n == 'snd   = FAUP.Snd <$> cnv ea
                  | n == 'len   = FAUP.Len <$> cnv ea
  cnv (TH.AppE (TH.AppE (TH.VarE n) el) er)
                  | n == 'ind   = FAUP.Ind <$> cnv el <*> cnv er 
  cnv (TH.AppE (TH.AppE (TH.VarE n) el) (TH.LamE [TH.VarP x] eb))
                  | n == 'ary   = FAUP.Ary <$> cnv el <*> pure x <*> cnv eb 
  cnv (TH.AppE (TH.AppE (TH.AppE (TH.VarE n) 
                                 (TH.LamE [TH.VarP xc] ec)) 
                                 (TH.LamE [TH.VarP xb] eb)) ei)
                  | n == 'whl   = FAUP.Whl <$> pure xc <*> cnv ec
                                  <*> pure xb <*> cnv eb <*> cnv ei
  cnv (TH.AppE ef ea)           = FAUP.App <$> cnv ef <*> cnv ea  
  cnv (TH.CondE ec et ef)       = FAUP.Cnd <$> cnv ec <*> cnv et <*> cnv ef
  cnv (TH.TupE [ef , es])       = FAUP.Tpl <$> cnv ef <*> cnv es
  cnv (TH.LetE [TH.ValD (TH.VarP x) (TH.NormalB el) []] eb) 
                                = FAUP.Let x <$> cnv el <*> cnv eb
  cnv _                         = fail "Syntax Error!"