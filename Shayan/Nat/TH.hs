module Nat.TH where

import MyPrelude
import Language.Haskell.TH.Syntax

nat :: Int -> String -> Q Exp
nat 0 p = do Just nm <- lookupValueName (p ++ "Zro")
             return (ConE nm)
nat n p = do Just nm <- lookupValueName (p ++ "Suc")
             AppE (ConE nm) <$> (nat (n - 1) p)

natP :: Int -> String -> Q Pat
natP 0 p = do Just nm <- lookupValueName (p ++ "Zro")
              return (ConP nm [])
natP n p = do Just nm <- lookupValueName (p ++ "Suc")
              sp <- natP (n - 1) p
              return (ConP nm [sp])

natT :: Int -> String -> Q Type
natT 0 p = do Just nm <- lookupValueName (p ++ "Zro")
              return (ConT nm)
natT n p = do Just nm <- lookupValueName (p ++ "Suc")
              AppT (ConT nm) <$> (natT (n - 1) p)