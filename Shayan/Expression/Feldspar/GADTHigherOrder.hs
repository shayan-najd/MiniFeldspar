module Expression.Feldspar.GADTHigherOrder where
 
import Prelude ()
import MyPrelude

import Variable.Typed

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import Singleton

data Exp :: [TFA.Typ] -> TFA.Typ -> * where 
  ConI :: Integer  -> Exp r TFA.Int 
  ConB :: Bool     -> Exp r TFA.Bol
  Var  :: Var r t  -> Exp r t  
  Abs  :: (Exp r ta -> Exp r tb) -> Exp r (TFA.Arr ta tb) 
  App  :: HasSin TFG.Typ ta => 
          Exp r (TFA.Arr ta tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r TFA.Bol -> Exp r t -> Exp r t -> Exp r t 
  Whl  :: (Exp r t -> Exp r TFA.Bol)-> (Exp r t -> Exp r t) -> Exp r t -> Exp r t
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (TFA.Tpl tf ts) 
  Fst  :: HasSin TFG.Typ ts => Exp r (TFA.Tpl tf ts) -> Exp r tf 
  Snd  :: HasSin TFG.Typ tf => Exp r (TFA.Tpl tf ts) -> Exp r ts 
  Ary  :: Exp r TFA.Int -> (Exp r TFA.Int -> Exp r ta) -> Exp r (TFA.Ary ta)
  Len  :: HasSin TFG.Typ ta => Exp r (TFA.Ary ta) -> Exp r TFA.Int 
  Ind  :: Exp r (TFA.Ary ta) -> Exp r TFA.Int -> Exp r ta 
  Let  :: HasSin TFG.Typ tl => Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb  
  
deriving instance Show (Exp r t)
instance Show (Exp r ta -> Exp r tb) where
  show = const "f"

sucAll :: Exp r t' -> Exp (t ': r) t' 
sucAll = mapVar Suc prd
                           
prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc
               
mapVar :: (forall t'. Var r  t' -> Var r' t') -> 
          (forall t'. Var r' t' -> Var r  t') -> 
          Exp r t -> Exp r' t
mapVar _ _ (ConI i)       = ConI i
mapVar _ _ (ConB i)       = ConB i
mapVar f _ (Var v)        = Var (f v)
mapVar f g (Abs eb)       = Abs (mapVar f g . eb .  mapVar g f)  
mapVar f g (App ef ea)    = App (mapVar f g ef) (mapVar f g ea)
mapVar f g (Cnd ec et ef) = Cnd (mapVar f g ec) (mapVar f g et) (mapVar f g ef)
mapVar f g (Whl ec eb ei) = Whl (mapVar f g . ec . mapVar g f) 
                                (mapVar f g . eb . mapVar g f) (mapVar f g ei)
mapVar f g (Tpl ef es)    = Tpl (mapVar f g ef) (mapVar f g es)
mapVar f g (Fst e)        = Fst (mapVar f g e)
mapVar f g (Snd e)        = Snd (mapVar f g e)
mapVar f g (Ary el ef)    = Ary (mapVar f g el) (mapVar f g . ef . mapVar g f)
mapVar f g (Len e)        = Len (mapVar f g e)
mapVar f g (Ind ea ei)    = Ind (mapVar f g ea) (mapVar f g ei)
mapVar f g (Let el eb)    = Let (mapVar f g el) (mapVar f g . eb . mapVar g f)