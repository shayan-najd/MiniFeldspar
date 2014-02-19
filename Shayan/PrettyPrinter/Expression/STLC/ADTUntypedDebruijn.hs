{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module PrettyPrinter.Expression.STLC.ADTUntypedDebruijn where

import Expression.STLC.ADTUntypedDebruijn

instance Show Exp where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs eb)              = "(\\ " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
