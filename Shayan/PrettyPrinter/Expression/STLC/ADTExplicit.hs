module PrettyPrinter.Expression.STLC.ADTExplicit where

import Expression.STLC.ADTExplicit
import PrettyPrinter.Nat ()
 
instance Show t => Show (Exp t) where 
  show (Con t i)                 = "(" ++ show i ++ " : " ++ show t ++ ")"
  show (Var t v)                 = "(" ++ show v ++ " : " ++ show t ++ ")"
  show (Abs t eb)                = "(\\ " ++ show eb ++ " : " ++ show t ++ ")"
  show (App t ef@(App _ _ _) ea) = "((" ++ show ef ++ ") " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (App t ef@(Add _ _ _) ea) = "((" ++ show ef ++ ") " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (App t ef ea)             = "(" ++ show ef ++ " " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (Add t el@(App _ _ _) er) = "((" ++ show el ++ ") + " ++ show er 
                                   ++ " : " ++ show t ++ ")"
  show (Add t el@(Add _ _ _) er) = "((" ++ show el ++ ") + " ++ show er 
                                   ++ " : " ++ show t ++ ")"
  show (Add t el er)             = "(" ++ show el ++ " + " ++ show er 
                                   ++ " : " ++ show t ++ ")"