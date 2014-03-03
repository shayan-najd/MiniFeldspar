module Expression.STLC.ADTUntypedNamed where
 
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..))
 
data Exp x =
    Con Integer
  | Var x
  | Abs x       (Exp x) 
  | App (Exp x) (Exp x)
  | Add (Exp x) (Exp x)
  deriving (Eq, Functor, Foldable, Traversable)