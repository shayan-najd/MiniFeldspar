{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.TypeSTLC where

import Prelude hiding (sin)
import qualified Type.STLC.GADT as G
import Singleton
import Control.Applicative ((<$>),(<*>))

instance Sin G.Typ Integer where
  sin = return G.Int 
 
instance (Sin G.Typ ta , Sin G.Typ tb) => Sin G.Typ (ta -> tb) where
  sin = G.Arr <$> sin <*> sin