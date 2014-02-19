{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs , FlexibleContexts  #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds #-}
module Expression.Feldspar.GADTFirstOrder where
 
import Prelude hiding (sin)
import Variable.GADT
import Singleton.TypeFeldspar
import Singleton
import qualified Type.Feldspar as A

data Exp :: [A.Typ] -> A.Typ -> * where 
  ConI :: Integer  -> Exp r A.Int 
  ConB :: Bool     -> Exp r A.Bol
  Var  :: Var r t  -> Exp r t  
  Abs  :: Typ ta -> Exp (ta ': r) tb -> Exp r (A.Arr ta tb) 
  App  :: Exp r (A.Arr ta tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r A.Bol ->  Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp r (A.Arr t A.Bol) -> Exp r (A.Arr t t) -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (A.Tpl tf ts) 
  Fst  :: Exp r (A.Tpl tf ts) -> Exp r tf 
  Snd  :: Exp r (A.Tpl tf ts) -> Exp r ts 
  Ary  :: Exp r A.Int -> Exp r (A.Arr A.Int t) -> Exp r (A.Ary t)
  Len  :: Exp r (A.Ary t) -> Exp r A.Int 
  Ind  :: Exp r (A.Ary t) -> Exp r A.Int -> Exp r t 
  Let  :: Typ tl -> Exp r tl -> Exp (tl ': r) tb -> Exp r tb  
  --  Any  :: Exp r t           

abs :: HasSin Typ ta => Exp (ta ': r) tb -> Exp r (A.Arr ta tb) 
abs = Abs sin  

lett :: HasSin Typ tl => Exp r tl -> Exp (tl ': r) tb -> Exp r tb  
lett = Let sin