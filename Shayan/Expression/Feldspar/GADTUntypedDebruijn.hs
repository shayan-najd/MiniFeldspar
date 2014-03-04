module Expression.Feldspar.GADTUntypedDebruijn where
 
import qualified Data.Nat      as A
import qualified Data.Fin  as F

data Exp :: A.Nat -> * where 
  ConI :: Integer -> Exp n 
  ConB :: Bool    -> Exp n
  Var  :: F.Nat n -> Exp n  
  Abs  :: Exp (A.Suc n)  -> Exp n
  App  :: Exp n -> Exp n -> Exp n  
  Cnd  :: Exp n -> Exp n -> Exp n -> Exp n 
  Whl  :: Exp (A.Suc n)  -> Exp (A.Suc n) -> Exp n -> Exp n
  Tpl  :: Exp n -> Exp n -> Exp n 
  Fst  :: Exp n -> Exp n 
  Snd  :: Exp n -> Exp n
  Ary  :: Exp n -> Exp (A.Suc n) -> Exp n  
  Len  :: Exp n -> Exp n  
  Ind  :: Exp n -> Exp n -> Exp n 
  Let  :: Exp n -> Exp (A.Suc n) -> Exp n 
  deriving Eq