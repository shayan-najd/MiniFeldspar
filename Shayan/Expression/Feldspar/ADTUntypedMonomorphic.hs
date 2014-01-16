{-# OPTIONS_GHC -Wall #-}
module Expression.Feldspar.ADTUntypedMonomorphic where
 
import Variable.ADT 

data Exp = ConI Integer 
         | ConB Bool 
         | Var Var  
         | Abs Exp
         | App Exp Exp  
         | Cnd Exp Exp Exp 
         | Whl Exp Exp Exp 
         | Tpl Exp Exp
         | Fst Exp 
         | Snd Exp
         | Ary Exp Exp
         | Len Exp 
         | Ind Exp Exp 
         | Let Exp Exp
         --         | Any
         deriving Eq

instance Show Exp where 
  show (ConI i)              = show i 
  show (ConB b)              = show b
  show (Var x)               = show x 
  show (Abs eb)              = "(\\ -> " ++ show eb ++ ")"
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
           