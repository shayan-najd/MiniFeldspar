module Expression.Feldspar.MiniWellScoped where
 
import Prelude ()
import MyPrelude

import qualified Type.Feldspar.ADT      as TFA
import qualified Type.Feldspar.GADT     as TFG

import Variable.Typed

import Environment.Typed     

import Singleton  

data Exp :: [TFA.Typ] -> TFA.Typ -> * where
  ConI  :: Integer  -> Exp r TFA.Int
  ConB  :: Bool     -> Exp r TFA.Bol
  AppV  :: HasSin TFG.Typ t =>
           Var r t  -> Env (Exp r) (TFG.Arg t) -> Exp r (TFG.Out t)
  Cnd   :: Exp r TFA.Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r t -> Exp r TFA.Bol) -> (Exp r t -> Exp r t) -> 
           Exp r t  -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (TFA.Tpl tf ts)
  Fst   :: HasSin TFG.Typ ts => 
           Exp r (TFA.Tpl tf ts)-> Exp r tf
  Snd   :: HasSin TFG.Typ tf => 
           Exp r (TFA.Tpl tf ts)-> Exp r ts
  Ary   :: Exp r TFA.Int -> (Exp r TFA.Int -> Exp r t) -> Exp r (TFA.Ary t)
  Len   :: HasSin TFG.Typ ta => 
           Exp r (TFA.Ary ta) -> Exp r TFA.Int
  Ind   :: Exp r (TFA.Ary ta) -> Exp r TFA.Int -> Exp r ta
  Let   :: HasSin TFG.Typ tl => 
           Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb