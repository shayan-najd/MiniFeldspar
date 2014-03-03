module PrettyPrinter.Expression.STLC.ADTChurch where

import Expression.STLC.ADTChurch
import PrettyPrinter.Nat ()

instance Show a => Show (Exp a) where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs t eb)            = "(\\" ++ show t ++ " -> " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er
