import Prelude hiding (sequence,mapM)
import Data.Foldable
import Data.Traversable

data Term a =
    Var a 
  | App (Term a) (Term a) 
  | Lam (Term (Maybe a))
    deriving (Eq,Show,Functor,Foldable)
 
instance Monad Term where 
  return = Var
  
  (Var x)     >>= f = f x
  (App t1 t2) >>= f = App (t1 >>= f) (t2 >>= f)
  (Lam tb)    >>= f = Lam (tb >>= mapM f)

freeVar :: Term a -> [a]
freeVar = toList
 
subst :: Term a -> Maybe a -> Term a
subst = flip maybe return

apply :: Term a -> Term (Maybe a) -> Term a
apply = (=<<) . subst 

abstract :: Eq a => a -> Term a -> Term a             
abstract = (Lam .) . fmap . match 

match :: Eq a => a -> a -> Maybe a
match x y = if x == y 
            then Nothing 
            else Just y
                  
beta :: Eq a => Term a -> Term a  
beta (App (Lam tb) t2) = apply t2 tb 
beta x                 = x