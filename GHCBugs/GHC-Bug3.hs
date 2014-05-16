import Data.Constraint
import Data.Constraint.Unsafe

data List :: [*] -> * where
  Nil   :: List '[]
  (:::) :: Maybe t -> List ts -> List (t ': ts)  

type family Append (xs :: [k]) (ys :: [k]) {- :: [k] -} where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

append :: List xs -> List ys -> List (Append xs ys)
append Nil        ys = ys  
append (x ::: xs) ys = x ::: (append xs ys)

append' :: forall ra rb. 
           List ra -> List rb -> List (Append ra rb)  
append' xs Nil                                                     = 
  case unsafeCoerceConstraint 
       :: () :- Append ra '[] ~ ra of 
    Sub Dict -> xs
append' xs ((y :: Maybe ty) ::: (ys :: List tys)) = 
  case unsafeCoerceConstraint 
       :: () :- Append (Append ra (ty ': '[])) tys ~ Append ra (ty ': tys) of 
    Sub Dict -> append' (append xs (y ::: Nil)) ys
                                                 
                                                 