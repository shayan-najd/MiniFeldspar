module Expression.Feldspar.Conversions.Unquoting () where

import MyPrelude (pure,fail,toRational,toInteger,fromInteger,fromRational
                 ,show,(++),(==),Maybe(..),(<$>),(<*>),ErrM)

import qualified Expression.Feldspar.ADTUntypedNamed as FAUN
import qualified Language.Haskell.TH.Syntax          as TH

import Conversion

import Examples.Feldspar.Prelude.TemplateHaskell

mkDo :: [TH.Stmt] -> ErrM (FAUN.Exp TH.Name)
mkDo st = let ?r = () in case st of
  [TH.NoBindS e]                -> cnvImp e
  (TH.BindS (TH.VarP x) e : es) -> FAUN.App <$>
                                   (FAUN.App (FAUN.Var (TH.mkName "bind"))
                                            <$> (FAUN.Abs <$@> x <*@> e ))
                                   <*> mkDo es
  (TH.NoBindS           e : es) -> FAUN.App <$>
                                   (FAUN.App (FAUN.Var (TH.mkName "bind"))
                                            <$> (FAUN.Abs
                                                         (TH.mkName "__dummyb__")
                                                         <$@> e ))
                                   <*> mkDo es
  _                             -> fail ("Syntax Error!\n" ++ (show st))

instance Cnv (TH.Exp , ()) (FAUN.Exp TH.Name) where
  cnv (ee , r) = let ?r = r in case ee of
    TH.ParensE e            -> cnvImp e
    TH.InfixE (Just el) ef
      (Just er)             -> cnvImp (TH.AppE (TH.AppE ef el) er)
    TH.LitE (TH.IntegerL i) -> FAUN.ConI <$@> (fromInteger  i :: Integer)
    TH.LitE (TH.RationalL i)-> FAUN.ConF <$@> (fromRational i :: Float)
    TH.ConE n
      | n == 'True          -> FAUN.ConB <$@> True
      | n == 'False         -> FAUN.ConB <$@> False
    TH.ConE n               -> FAUN.Var  <$@> n
    TH.VarE n               -> FAUN.Var  <$@> n
    TH.LamE [TH.VarP x] eb  -> FAUN.Abs  <$@> x <*@> eb
    TH.DoE stmts            -> mkDo stmts
    TH.AppE (TH.VarE n) ea
      | n == 'fst           -> FAUN.Fst  <$@> ea
      | n == 'snd           -> FAUN.Snd  <$@> ea
      | n == 'len           -> FAUN.Len  <$@> ea
    TH.AppE (TH.AppE
        (TH.VarE n) el) er
      | n == 'ind           -> FAUN.Ind  <$@> el <*@> er
      | n == 'cmx           -> FAUN.Cmx  <$@> el <*@> er
    TH.AppE (TH.AppE
             (TH.VarE n) el)
      (TH.LamE [TH.VarP x] eb)
      | n == 'ary           -> FAUN.Ary  <$@> el <*@> x <*@> eb
    TH.AppE (TH.AppE
        (TH.AppE (TH.VarE n)
        (TH.LamE [TH.VarP xc] ec))
        (TH.LamE [TH.VarP xb] eb))
        ei
      | n == 'whl           -> FAUN.Whl  <$@> xc <*@> ec <*@> xb
                                         <*@> eb <*@> ei
    TH.AppE ef ea           -> FAUN.App  <$@> ef <*@> ea
    TH.CondE ec et ef       -> FAUN.Cnd  <$@> ec <*@> et <*@> ef
    TH.TupE [ef , es]       -> FAUN.Tpl  <$@> ef <*@> es
    TH.LetE [TH.ValD (TH.VarP x) (TH.NormalB el) []] eb
                            -> FAUN.Let  <$@> x  <*@> el <*@> eb
    e                       -> fail  ("Syntax Error!\n" ++ (show e))

instance Cnv (FAUN.Exp TH.Name , ()) TH.Exp where
  cnv (ee , r) = let ?r = r in case ee of
    FAUN.ConI i             -> pure (TH.LitE (TH.IntegerL (toInteger i)))
    FAUN.ConB True          -> pure (TH.ConE 'True)
    FAUN.ConB False         -> pure (TH.ConE 'False)
    FAUN.ConF i             -> pure (TH.LitE (TH.RationalL (toRational i)))
    FAUN.Var n              -> pure (TH.VarE n)
    FAUN.Abs x  eb          -> TH.LamE [TH.VarP x] <$@> eb
    FAUN.App ef ea          -> TH.AppE <$@> ef <*@> ea
    FAUN.Cnd ec et ef       -> TH.CondE <$@> ec <*@> et <*@> ef
    FAUN.Whl xc ec xb eb ei -> do ec' <- cnvImp ec
                                  eb' <- cnvImp eb
                                  ei' <- cnvImp ei
                                  pure (TH.AppE (TH.AppE
                                                 (TH.AppE (TH.VarE 'whl)
                                                  (TH.LamE [TH.VarP xc] ec'))
                                                 (TH.LamE [TH.VarP xb] eb'))
                                        ei')
    FAUN.Tpl ef es          -> do ef' <- cnvImp ef
                                  es' <- cnvImp es
                                  pure (TH.TupE [ef' , es'])
    FAUN.Fst ea             -> TH.AppE (TH.VarE 'fst) <$@> ea
    FAUN.Snd ea             -> TH.AppE (TH.VarE 'snd) <$@> ea
    FAUN.Ary el x eb        -> do el' <- cnvImp el
                                  eb' <- cnvImp eb
                                  pure (TH.AppE (TH.AppE (TH.VarE 'ary) el')
                                                 (TH.LamE [TH.VarP x] eb'))
    FAUN.Len ea             -> TH.AppE (TH.VarE 'len) <$@> ea
    FAUN.Ind el ef          -> do el' <- cnvImp el
                                  ef' <- cnvImp ef
                                  pure (TH.AppE (TH.AppE (TH.VarE 'ind) el') ef')
    FAUN.Let x el eb        -> do el' <- cnvImp el
                                  eb' <- cnvImp eb
                                  pure (TH.LetE [TH.ValD (TH.VarP x)
                                         (TH.NormalB el') []] eb')
    FAUN.Cmx er ei          -> do er' <- cnvImp er
                                  ei' <- cnvImp ei
                                  pure (TH.AppE (TH.AppE (TH.VarE 'cmx) er') ei')