{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expression.Feldspar.ADTUntypedNamed where
 
import Data.Traversable
import Data.Foldable

data Exp x = ConI Integer 
           | ConB Bool 
           | Var x  
           | Abs x (Exp x)
           | App (Exp x) (Exp x)  
           | Cnd (Exp x) (Exp x) (Exp x) 
           | Whl (Exp x) (Exp x) (Exp x) 
           | Tpl (Exp x) (Exp x)
           | Fst (Exp x) 
           | Snd (Exp x)
           | Ary (Exp x) (Exp x)
           | Len (Exp x) 
           | Ind (Exp x) (Exp x) 
           | Let x (Exp x) (Exp x) 
           --           | Any
           deriving (Eq,Functor,Foldable,Traversable)

instance Show x => Show (Exp x) where 
  show (ConI i)              = show i 
  show (ConB b)              = show b
  show (Var x)               = show x 
  show (Abs x eb)            = "(\\ " ++ show x ++ " ->" ++ show eb ++ ")"
  show (App ef ea)           = show ef ++ " (" ++ show ea ++ ")"
  show (Cnd ec et ef)        = "if " ++ show ec ++ 
                               " then " ++ show et ++ 
                               " else " ++ show ef
  show (Whl ec eb ei)        = "while " ++ show ec ++ 
                               " from " ++ show ei ++ 
                               " do "   ++ show eb
  show (Tpl ef es)           = "(" ++ show ef 
                               ++ " , " ++ show es ++ ")"                     
  show (Fst e)               = "fst " ++ show e                              
  show (Snd e)               = "snd " ++ show e                              
  show (Ary el ef)           = "mkArr " ++ show el ++ " " ++ show ef
--  show Any                   = "undefined"
  show (Len e)               = "len " ++ show e
  show (Ind ea ei)           = show ea ++ " ! " ++ show ei
  show (Let x el eb)         = "let " ++ show x ++ " = " ++ show el ++ 
                               " in " ++ show eb
  