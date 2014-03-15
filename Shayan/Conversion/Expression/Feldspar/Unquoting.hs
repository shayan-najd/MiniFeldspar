module Conversion.Expression.Feldspar.Unquoting () where

import Prelude   ()
import MyPrelude ((==),fail,Bool(..),Maybe(..),pure)

import Conversion
import qualified Expression.Feldspar.ADTUntypedNamed as FAUP
import qualified Language.Haskell.TH.Syntax          as TH
import VanillaPrelude

instance Cnv (TH.Exp , ()) (FAUP.Exp TH.Name) where
  cnv (ee , ()) = let ?r = () in case ee of 
    TH.ParensE e            -> cnv (e , ())
    TH.InfixE (Just el) ef 
      (Just er)             -> cnv (TH.AppE (TH.AppE ef el) er , ())
    TH.LitE (TH.IntegerL i) -> FAUP.ConI <$@> i
    TH.ConE n 
      | n == 'True          -> FAUP.ConB <$@> True
      | n == 'False         -> FAUP.ConB <$@> False
    TH.VarE n               -> FAUP.Var  <$@> n
    TH.LamE [TH.VarP x] eb  -> FAUP.Abs  <$@> x <*@> eb 
    TH.AppE (TH.VarE n) ea 
      | n == 'fst           -> FAUP.Fst  <$@> ea
      | n == 'snd           -> FAUP.Snd  <$@> ea
      | n == 'len           -> FAUP.Len  <$@> ea
    TH.AppE (TH.AppE 
        (TH.VarE n) el) er
      | n == 'ind           -> FAUP.Ind  <$@> el <*@> er 
    TH.AppE (TH.AppE 
             (TH.VarE n) el) 
      (TH.LamE [TH.VarP x] eb)
      | n == 'ary           -> FAUP.Ary  <$@> el <*@> x <*@> eb 
    TH.AppE (TH.AppE 
        (TH.AppE (TH.VarE n) 
        (TH.LamE [TH.VarP xc] ec)) 
        (TH.LamE [TH.VarP xb] eb)) 
        ei
      | n == 'whl           -> FAUP.Whl  <$@> xc <*@> ec <*@> xb 
                                         <*@> eb <*@> ei
    TH.AppE ef ea           -> FAUP.App  <$@> ef <*@> ea  
    TH.CondE ec et ef       -> FAUP.Cnd  <$@> ec <*@> et <*@> ef
    TH.TupE [ef , es]       -> FAUP.Tpl  <$@> ef <*@> es
    TH.LetE [TH.ValD (TH.VarP x) (TH.NormalB el) []] eb 
                            -> FAUP.Let  <$@> x  <*@> el <*@> eb
    _                       -> fail "Syntax Error!"
    
instance Cnv (FAUP.Exp TH.Name , ()) TH.Exp where
  cnv (ee , ()) = let ?r = () in case ee of 
    FAUP.ConI i             -> pure (TH.LitE (TH.IntegerL i))
    FAUP.ConB True          -> pure (TH.ConE 'True)
    FAUP.ConB False         -> pure (TH.ConE 'False)
    FAUP.Var n              -> pure (TH.VarE n)
    FAUP.Abs x  eb          -> TH.LamE [TH.VarP x]    <$@> eb
    FAUP.App ef ea          -> TH.AppE <$@> ef <*@> ea  
    FAUP.Cnd ec et ef       -> TH.CondE <$@> ec <*@> et <*@> ef
    FAUP.Whl xc ec xb eb ei -> do ec' <- cnv (ec , ())
                                  eb' <- cnv (eb , ())
                                  ei' <- cnv (ei , ())
                                  pure (TH.AppE (TH.AppE 
                                                 (TH.AppE (TH.VarE 'whl) 
                                                  (TH.LamE [TH.VarP xc] ec')) 
                                                 (TH.LamE [TH.VarP xb] eb')) 
                                        ei')
    FAUP.Tpl ef es          -> do ef' <- cnv (ef , ())
                                  es' <- cnv (es , ())       
                                  pure (TH.TupE [ef' , es']) 
    FAUP.Fst ea             -> TH.AppE (TH.VarE 'fst) <$@> ea
    FAUP.Snd ea             -> TH.AppE (TH.VarE 'snd) <$@> ea
    FAUP.Ary el x eb        -> do el' <- cnv (el , ())
                                  eb' <- cnv (eb , ())
                                  pure (TH.AppE (TH.AppE (TH.VarE 'ary) el') 
                                                 (TH.LamE [TH.VarP x] eb'))
    FAUP.Len ea             -> TH.AppE (TH.VarE 'len) <$@> ea
    FAUP.Ind el ef          -> do el' <- cnv (el , ())
                                  ef' <- cnv (ef , ())       
                                  pure (TH.AppE (TH.AppE (TH.VarE 'ind) el') ef')
    FAUP.Let x el eb        -> do el' <- cnv (el , ())
                                  eb' <- cnv (eb , ())
                                  pure (TH.LetE [TH.ValD (TH.VarP x) 
                                         (TH.NormalB el') []] eb')