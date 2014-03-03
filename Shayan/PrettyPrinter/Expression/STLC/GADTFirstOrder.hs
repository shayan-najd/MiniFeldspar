module PrettyPrinter.Expression.STLC.GADTFirstOrder where

import Expression.STLC.GADTFirstOrder
import PrettyPrinter.Variable ()

instance Show (Exp r t) where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs _ eb)            = "(\\ " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
 