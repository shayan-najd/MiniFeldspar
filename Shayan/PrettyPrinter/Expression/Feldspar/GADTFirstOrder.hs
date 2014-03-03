module PrettyPrinter.Expression.Feldspar.GADTFirstOrder where
 
import Expression.Feldspar.GADTFirstOrder
import PrettyPrinter.Variable ()

instance Show (Exp r t) where 
  show (ConI i)              = show i 
  show (ConB b)              = show b
  show (Var x)               = show x 
  show (Abs eb)              = "(\\ -> " ++ show eb ++ ")"
  show (App _ ef ea)         = show ef ++ " (" ++ show ea ++ ")"
  show (Cnd ec et ef)        = "if " ++ show ec ++ 
                               " then " ++ show et ++ 
                               " else " ++ show ef
  show (Whl ec eb ei)        = "while " ++ show ec ++ 
                               " from " ++ show ei ++ 
                               " do "   ++ show eb
  show (Tpl ef es)           = "(" ++ show ef 
                               ++ " , " ++ show es ++ ")"                     
  show (Fst _ e)               = "fst " ++ show e                              
  show (Snd _ e)               = "snd " ++ show e                              
  show (Ary el ef)           = "mkArr " ++ show el ++ " " ++ show ef
  show (Len _ e)               = "len " ++ show e
  show (Ind ea ei)           = show ea ++ " ! " ++ show ei
  show (Let _ el eb)         = "let " ++ show el ++
                               " in " ++ show eb