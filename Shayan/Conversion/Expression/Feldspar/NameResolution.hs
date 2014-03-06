module Conversion.Expression.Feldspar.NameResolution where

import qualified Expression.Feldspar.ADTUntypedNamed    as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Data.Nat                               as A
import qualified Environment.ADTTable                   as AT
import qualified Environment.ADT                        as A
import Conversion
import Conversion.Nat () 

instance Eq x => 
         Cnv (FAUP.Exp x , A.Env x) FAUM.Exp where
  cnv (e , rt) = cnv (e , zip rt [A.Zro ..])
     
instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x A.Nat) FAUM.Exp where
  cnv (eaup , rb) = let ?cnv = cnv' in case eaup of
        FAUP.ConI i             -> FAUM.ConI <$> pure i
        FAUP.ConB b             -> FAUM.ConB <$> pure b
        FAUP.Var s              -> FAUM.Var  <$> AT.get s rb
        FAUP.Abs xb eb          -> FAUM.Abs  <$> cnv ((xb , eb), rb)
        FAUP.App ef ea          -> FAUM.App  <$@> ef <*@> ea
        FAUP.Cnd ec et ef       -> FAUM.Cnd  <$@> ec <*@> et <*@> ef 
        FAUP.Whl xc ec xb eb ei -> FAUM.Whl  <$>  cnv ((xc , ec), rb) 
                                             <*>  cnv ((xb , eb), rb) 
                                             <*@> ei 
        FAUP.Tpl ef es          -> FAUM.Tpl <$@> ef <*@> es 
        FAUP.Fst e              -> FAUM.Fst <$@> e 
        FAUP.Snd e              -> FAUM.Snd <$@> e 
        FAUP.Ary el xf ef       -> FAUM.Ary <$@> el <*> cnv ((xf , ef), rb)
        FAUP.Ind ea ei          -> FAUM.Ind <$@> ea <*@> ei 
        FAUP.Len e              -> FAUM.Len <$@> e
        FAUP.Let xl el eb       -> FAUM.Let <$@> el <*> cnv ((xl , eb), rb)
        where
          cnv' :: Cnv (e , AT.Env x A.Nat) FAUM.Exp => 
                  e -> ErrM FAUM.Exp 
          cnv' e = cnv (e , rb)           

instance Eq x => 
         Cnv ((x , FAUP.Exp x) , AT.Env x A.Nat) 
         FAUM.Exp where
  cnv ((x , e) , rb) = cnv (e , (x , A.Zro) : fmap A.Suc `map` rb)