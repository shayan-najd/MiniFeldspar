module Nat.TH where

import MyPrelude
import Language.Haskell.TH.Syntax

nat :: Int -> String -> Q Exp
nat 0 p = do Just nm <- lookupValueName (p ++ "Zro")
             return (ConE nm)
nat n p = do Just nm <- lookupValueName (p ++ "Suc")
             AppE (ConE nm) <$> (nat (n - 1) p) 
   
