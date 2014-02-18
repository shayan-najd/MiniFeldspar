{-# OPTIONS_GHC -Wall #-}
module Expression.Feldspar.ADTUntypedDebruijn where
 
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
         deriving Eq