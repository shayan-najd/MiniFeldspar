{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , GADTs, ScopedTypeVariables, PolyKinds #-}
module Conversion.Existential where

import Prelude hiding (sin)
import Conversion
import Existential
import Singleton
import SingletonEquality

---------------------------------------------------------------------------------
-- Conversion from Existentials
---------------------------------------------------------------------------------
instance (EqlSin st, HasSin st t) =>
          Cnv (Exs0 st) t where
  cnv (Exs0 e t) = do Rfl <- eqlSin t (sin :: st t)         
                      return e

instance (EqlSin st, HasSin st t) =>
          Cnv (ExsSin st) (st t) where
  cnv (ExsSin t) = do Rfl <- eqlSin t (sin :: st t)     
                      return t

instance (EqlSin st, HasSin st t) =>
          Cnv (Exs1 e st) (e t) where
  cnv (Exs1 e t) = do Rfl <- eqlSin t (sin :: st t)     
                      return e

instance (EqlSin sr, EqlSin st, HasSin st t , HasSin sr r) =>
         Cnv (Exs2 e sr st) (e r t) where
  cnv (Exs2 e r t) = do Rfl <- eqlSin t (sin :: st t)     
                        Rfl <- eqlSin r (sin :: sr r)
                        return e

