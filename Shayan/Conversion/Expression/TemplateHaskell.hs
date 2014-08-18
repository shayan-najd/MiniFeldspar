module Conversion.Expression.TemplateHaskell () where

import MyPrelude

import qualified Language.Haskell.TH.Syntax as TH

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
