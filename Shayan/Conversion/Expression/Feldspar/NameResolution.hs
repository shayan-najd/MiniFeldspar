module Conversion.Expression.Feldspar.NameResolution where

import Prelude hiding (sin)
import qualified Expression.Feldspar.ADTUntypedNamed as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Type.Feldspar as FAS
import qualified Data.Nat               as A
import qualified Environment.ADTTable    as AT
import Conversion
import Conversion.Nat () 

instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUM.Exp) FAUM.Exp where
  cnv = \ (e , rt , rf) -> cnvv e (zip (map fst rt) [A.Zro ..]) rf
    where
      cnvv eaup rb rf = case eaup of
        FAUP.ConI i       -> FAUM.ConI <$> pure i
        FAUP.ConB b       -> FAUM.ConB <$> pure b
        FAUP.Var s        -> case (AT.get s rb , AT.get s rf) of
          (Just x  , _)      -> FAUM.Var <$> pure x
          (Nothing , Just e) -> pure e
          _                  -> fail "Scope Error!"
        FAUP.Abs s  eb    -> FAUM.Abs <$> 
                             cnvv eb ((s,A.Zro) : fmap A.Suc `map` rb) rf 
        FAUP.Let s el eb  -> FAUM.Let <$@> el <*> 
                             cnvv eb ((s,A.Zro) : fmap A.Suc `map` rb) rf 
        FAUP.App ef ea    -> FAUM.App <$@> ef <*@> ea
        FAUP.Cnd ec et ef -> FAUM.Cnd <$@> ec <*@> et <*@> ef 
        FAUP.Whl ec eb ei -> FAUM.Whl <$@> ec <*@> eb <*@> ei 
        FAUP.Tpl ef es    -> FAUM.Tpl <$@> ef <*@> es 
        FAUP.Fst e        -> FAUM.Fst <$@> e 
        FAUP.Snd e        -> FAUM.Snd <$@> e 
        FAUP.Ary el ef    -> FAUM.Ary <$@> el <*@> ef 
        FAUP.Ind ea ei    -> FAUM.Ind <$@> ea <*@> ei 
        FAUP.Len e        -> FAUM.Len <$@> e
        where
          ?cnv = \ e -> cnvv e rb rf   

