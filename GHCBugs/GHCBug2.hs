{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Variable.ADT(module N,Var) where

import Data.Nat as N

-- Variables are represented as natural numbers
type Var = Nat
    

-- adding space between "ADT" and "(" in the module header "ADT(" fixes the bug!
