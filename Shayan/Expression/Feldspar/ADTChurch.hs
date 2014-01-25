{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module Expression.Feldspar.ADTChurch where
 
import Data.Traversable
import Data.Foldable
import Variable.ADT

data Exp t = ConI Integer 
           | ConB Bool 
           | Var Var  
           | Abs t (Exp t)
           | App (Exp t) (Exp t)  
           | Cnd (Exp t) (Exp t) (Exp t) 
           | Whl (Exp t) (Exp t) (Exp t) 
           | Tpl (Exp t) (Exp t)
           | Fst (Exp t) 
           | Snd (Exp t)
           | Ary (Exp t) (Exp t)
           | Len (Exp t) 
           | Ind (Exp t) (Exp t) 
           | Let (Exp t) (Exp t) 
           --           | Any
           deriving (Eq,Functor,Foldable,Traversable)

instance Show x => Show (Exp x) where 
  show (ConI i)              = show i 
  show (ConB b)              = show b
  show (Var x)               = show x 
  show (Abs t eb)            = "(\\ " ++ show t ++ " -> " ++ show eb ++ ")"
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
  show (Let el eb)           = "let " ++ show el ++ 
                               " in " ++ show eb
