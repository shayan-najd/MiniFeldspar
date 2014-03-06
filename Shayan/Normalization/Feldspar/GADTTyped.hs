module Normalization.Feldspar.GADTTyped where

import Expression.Feldspar.GADTTyped
import Control.Applicative.Recursion
import qualified Data.Fin as F
import ChangeMonad

nrm :: Exp n t-> Exp n t
nrm = tilNotChg nrmOne
    
nrmOne :: Exp n t -> Chg (Exp n t)
nrmOne ee = case ee of
  ConI i              -> pure (ConI i)
  ConB b              -> pure (ConB b)
  Var x               -> pure (Var  x)
  Abs (App _ ef (Var F.Zro))
    | notElem F.Zro (fre ef) -> chg (prdAll ef)
  Abs eb              -> Abs <$> nrmOne eb
  App _  (Abs eb) ea  -> chg (prdAll (sbs eb F.Zro (sucAll ea)))
  App t  ef       ea  -> App <$> pure t <*@> ef <*@> ea    
  Cnd (ConB b) et ef  -> chg (if b then et else ef)
  Cnd ec et ef        -> Cnd <$@> ec <*@> et <*@> ef
  Whl ec eb ei        -> Whl <$> nrmOne ec <*> nrmOne eb <*@> ei
  Tpl ef es           -> Tpl <$@> ef <*@> es
  Fst _  (Tpl ef _)   -> chg ef
  Fst t  e            -> Fst <$> pure t <*@> e
  Snd _  (Tpl _ es)   -> chg es
  Snd t  e            -> Snd <$> pure t <*@> e                       
  Ary el ef           -> Ary <$@> el <*> nrmOne ef
  Len _  (Ary el _ )  -> chg el                         
  Len t  e            -> Len <$> pure t <*@> e                         
  Ind (Ary _  ef) ei  -> chg (App undefined (Abs ef) ei)                         
  Ind ea ei           -> Ind <$@> ea <*@> ei                         
  Let t  el eb        -> Let <$> pure t <*@> el <*> nrmOne eb
  where 
    ?cnv = nrmOne