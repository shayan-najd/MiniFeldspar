{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.TypeFeldspar where

import Prelude hiding (sin)
import qualified Type.Feldspar.GADT as G
import Singleton
import Data.Array (Array)

instance Sin G.Typ Integer where
  sin = G.Int 
 
instance Sin G.Typ Bool where
  sin = G.Bol 

instance (Sin G.Typ ta , Sin G.Typ tb) => Sin G.Typ (ta -> tb) where
  sin = G.Arr sin sin
  
instance (Sin G.Typ tf , Sin G.Typ ts) => Sin G.Typ (tf , ts) where
  sin = G.Tpl sin sin  
  
instance Sin G.Typ ta => Sin G.Typ (Array Integer ta) where
  sin = G.Ary sin  