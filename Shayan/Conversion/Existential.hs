{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , GADTs, ScopedTypeVariables #-}
module Conversion.Existential where

import Prelude hiding (sin)
import Conversion
import Existential
import Singleton
import SingletonEquality

---------------------------------------------------------------------------------
-- Conversion from Existentials
---------------------------------------------------------------------------------
instance (EqlSin st, Sin st t) =>
          Cnv (Exs0 st) t where
  cnv (Exs0 e t) = do t' :: st t <- sin                        
                      Rfl <- eqlSin t t'     
                      return e

instance (EqlSin st, Sin st t) =>
          Cnv (ExsSin st) (st t) where
  cnv (ExsSin t) = do t' :: st t <- sin                        
                      Rfl <- eqlSin t t'     
                      return t

instance (EqlSin st, Sin st t) =>
          Cnv (Exs1 e st) (e t) where
  cnv (Exs1 e t) = do t' :: st t <- sin                        
                      Rfl <- eqlSin t t'     
                      return e

instance (EqlSin sr, EqlSin st, Sin st t , Sin sr r) =>
         Cnv (Exs2 e sr st) (e r t) where
  cnv (Exs2 e r t) = do t' :: st t <- sin 
                        r' :: sr r <- sin 
                        Rfl <- eqlSin t t'     
                        Rfl <- eqlSin r r'
                        return e

