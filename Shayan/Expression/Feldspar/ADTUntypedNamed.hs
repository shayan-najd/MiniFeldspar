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
           deriving (Eq,Functor,Foldable,Traversable)