{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, Rank2Types #-}
module Examples.TemplateHaskell where

import Language.Haskell.TH.Syntax

dbl     :: Q (TExp (Integer -> Integer))
dbl     = [||\ x -> x + x ||]

compose :: Q (TExp ((tb -> tc) -> (ta -> tb) -> ta -> tc))
compose = [|| \ x2 -> \ x1 -> \ x0 -> x2 (x1 x0) ||]

four   :: Q (TExp Integer)
four    = [|| ($$compose $$dbl $$dbl) 1 ||]  

