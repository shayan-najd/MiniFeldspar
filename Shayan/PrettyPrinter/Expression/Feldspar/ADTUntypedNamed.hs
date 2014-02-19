{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module PrettyPrinter.Expression.Feldspar.ADTUntypedNamed where
 
import Expression.Feldspar.ADTUntypedNamed 

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
  show (Len e)               = "len " ++ show e
  show (Ind ea ei)           = show ea ++ " ! " ++ show ei
  show (Let x el eb)         = "let " ++ show x ++ " = " ++ show el ++ 
                               " in " ++ show eb
  