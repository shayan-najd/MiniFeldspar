module Conversion.Expression.Feldspar.NameResolution where

import qualified Expression.Feldspar.ADTUntypedNamed    as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Type.Feldspar                          as FAS
import qualified Data.Nat                               as A
import qualified Environment.ADTTable                   as AT
import Conversion
import Conversion.Nat () 

instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUM.Exp) FAUM.Exp where
  cnv (e , rt , rf) = cnv (e , zip (map fst rt) [A.Zro ..] , rf)
     
instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x A.Nat , AT.Env x FAUM.Exp) 
         FAUM.Exp where
  cnv (eaup , rb , rf) = let ?cnv = cnv' in case eaup of
        FAUP.ConI i             -> FAUM.ConI <$> pure i
        FAUP.ConB b             -> FAUM.ConB <$> pure b
        FAUP.Var s              -> case (AT.get s rb , AT.get s rf) of
          (Just x  , _)         -> FAUM.Var <$> pure x
          (Nothing , Just e)    -> pure e
          _                     -> fail "Scope Error!"
        FAUP.Abs xb eb          -> FAUM.Abs <$> cnv ((xb , eb), rb , rf)
        FAUP.App ef ea          -> FAUM.App <$@> ef <*@> ea
        FAUP.Cnd ec et ef       -> FAUM.Cnd <$@> ec <*@> et <*@> ef 
        FAUP.Whl xc ec xb eb ei -> FAUM.Whl <$>  cnv ((xc , ec), rb , rf) 
                                            <*>  cnv ((xb , eb), rb , rf) 
                                            <*@> ei 
        FAUP.Tpl ef es          -> FAUM.Tpl <$@> ef <*@> es 
        FAUP.Fst e              -> FAUM.Fst <$@> e 
        FAUP.Snd e              -> FAUM.Snd <$@> e 
        FAUP.Ary el xf ef       -> FAUM.Ary <$@> el <*> cnv ((xf , ef), rb , rf)
        FAUP.Ind ea ei          -> FAUM.Ind <$@> ea <*@> ei 
        FAUP.Len e              -> FAUM.Len <$@> e
        FAUP.Let xl el eb       -> FAUM.Let <$@> el <*> cnv ((xl , eb), rb , rf)
        where
          cnv' :: Cnv (e , AT.Env x A.Nat , AT.Env x FAUM.Exp) FAUM.Exp => 
                  e -> ErrM FAUM.Exp 
          cnv' e = cnv (e , rb , rf)           

instance Eq x => 
         Cnv ((x , FAUP.Exp x) , AT.Env x A.Nat , AT.Env x FAUM.Exp) 
         FAUM.Exp where
  cnv ((x , e) , rb , rf) = cnv (e , (x , A.Zro) : fmap A.Suc `map` rb , rf)