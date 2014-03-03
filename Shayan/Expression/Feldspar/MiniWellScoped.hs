module Expression.Feldspar.MiniWellScoped where

import Prelude (Integer , Bool)
import Singleton
import qualified Variable               as V 
import qualified Type.Feldspar          as F
import qualified Singleton.TypeFeldspar as FG
import qualified Singleton.Environment  as G
 
type family Out (t :: F.Typ) :: F.Typ where 
  Out (F.Arr ta tb) = Out tb
  Out t             = t
    
type family Arg (t :: F.Typ) :: [F.Typ] where
  Arg (F.Arr ta tb) = ta ': Arg tb
  Arg t             = '[]
 
data Exp :: [F.Typ] -> F.Typ -> * where
  ConI  :: Integer -> Exp r F.Int
  ConB  :: Bool -> Exp r F.Bol
  AppV  :: FG.Typ t ->
           V.Var r t -> G.Env (Exp r) (Arg t) -> Exp r (Out t)
  Cnd   :: Exp r F.Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r t -> Exp r F.Bol) -> (Exp r t -> Exp r t) -> 
           Exp r t -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (F.Tpl tf ts)
  Fst   :: FG.Typ ts -> Exp r (F.Tpl tf ts) -> Exp r tf
  Snd   :: FG.Typ tf -> Exp r (F.Tpl tf ts) -> Exp r ts
  Ary   :: Exp r F.Int -> (Exp r F.Int -> Exp r t) -> Exp r (F.Ary t)
  Len   :: FG.Typ ta -> Exp r (F.Ary ta) -> Exp r F.Int
  Ind   :: Exp r (F.Ary ta) -> Exp r F.Int -> Exp r ta
  Let   :: FG.Typ tl -> Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb

appV :: HasSin FG.Typ t => V.Var r t -> G.Env (Exp r) (Arg t) -> Exp r (Out t)
appV = AppV sin
 
fst :: HasSin FG.Typ ts => Exp r (F.Tpl tf ts) -> Exp r tf  
fst = Fst sin

snd :: HasSin FG.Typ tf => Exp r (F.Tpl tf ts) -> Exp r ts
snd = Snd sin

len :: HasSin FG.Typ ta => Exp r (F.Ary ta) -> Exp r F.Int
len = Len sin

lett :: HasSin FG.Typ tl => Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
lett = Let sin