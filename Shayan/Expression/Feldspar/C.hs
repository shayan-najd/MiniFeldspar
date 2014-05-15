module Expression.Feldspar.C where

import Prelude ()
import MyPrelude 

import qualified Type.Feldspar.ADT as TFA
 
type Var = (String , TFA.Typ)

data Func = Func String [Var] Stmt

data Stmt = 
   If  Exp Stmt Stmt
 | Whl Exp Stmt  
 | Assign String Exp
 | Declare Var
 | Grp [Stmt]  
 deriving Eq
  
data Exp = 
   Var String
 | Num Integer
 | Flt Float  
 | App String [Exp]  
 deriving Eq

