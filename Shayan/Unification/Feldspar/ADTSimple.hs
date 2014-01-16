{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances  #-}
module Unification.Feldspar.ADTSimple where

import qualified Type.Feldspar.ADTSimple as FS
import Unification
import ErrorMonad
 
instance Uni FS.Typ ErrM where
  tCon "Int" []         = return FS.Int
  tCon "Bol" []         = return FS.Bol
  tCon "Arr" [ta , tb]  = return (FS.Arr ta tb)
  tCon "Tpl" [tf , ts]  = return (FS.Tpl tf ts)
  tCon "Ary" [t]        = return (FS.Ary t)
  tCon _     _          = fail "Type Error!"
  
  eql = (FS.===)
  
  eqlCon "Int" FS.Int         = return []
  eqlCon "Bol" FS.Bol         = return []                      
  eqlCon "Arr" (FS.Arr ta tb) = return [ta , tb]
  eqlCon "Tpl" (FS.Tpl tf ts) = return [tf , ts] 
  eqlCon "Ary" (FS.Ary ta)    = return [ta]                      
  eqlCon _    _  = fail "Type Error!"                      
