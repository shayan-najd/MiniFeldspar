{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module PrettyPrinter.STLC.ADTUntypedNamed where
 
import Expression.STLC.ADTUntypedNamed 

instance Show x => Show (Exp x) where 
  show (Con i)               = show i 
  show (Var x)               = show x 
  show (Abs x eb)            = "(\\ " ++ show x ++ " ->" ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
