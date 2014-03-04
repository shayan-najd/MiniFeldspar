{-# LANGUAGE Rank2Types #-}
module Expression.Feldspar.GADTFirstOrder where
 
import Prelude hiding (sin)
import Variable
import qualified Singleton.TypeFeldspar as G
import Singleton
import qualified Type.Feldspar as A

data Exp :: [A.Typ] -> A.Typ -> * where 
  ConI :: Integer  -> Exp r A.Int 
  ConB :: Bool     -> Exp r A.Bol
  Var  :: Var r t  -> Exp r t  
  Abs  :: Exp (ta ': r) tb -> Exp r (A.Arr ta tb) 
  App  :: G.Typ ta -> Exp r (A.Arr ta tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r A.Bol ->  Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp (t ': r) A.Bol -> Exp (t ': r) t -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (A.Tpl tf ts) 
  Fst  :: G.Typ ts -> Exp r (A.Tpl tf ts) -> Exp r tf 
  Snd  :: G.Typ tf -> Exp r (A.Tpl tf ts) -> Exp r ts 
  Ary  :: Exp r A.Int -> Exp (A.Int ': r) t -> Exp r (A.Ary t)
  Len  :: G.Typ ta -> Exp r (A.Ary ta) -> Exp r A.Int 
  Ind  :: Exp r (A.Ary ta) -> Exp r A.Int -> Exp r ta 
  Let  :: G.Typ tl -> Exp r tl -> Exp (tl ': r) tb -> Exp r tb  

app :: HasSin G.Typ ta => Exp r (A.Arr ta tb) -> Exp r ta -> Exp r tb     
app = App sin

fst :: HasSin G.Typ ts => Exp r (A.Tpl tf ts) -> Exp r tf 
fst = Fst sin

snd :: HasSin G.Typ tf => Exp r (A.Tpl tf ts) -> Exp r ts 
snd = Snd sin

len :: HasSin G.Typ ta => Exp r (A.Ary ta) -> Exp r A.Int 
len = Len sin

lett :: HasSin G.Typ tl => Exp r tl -> Exp (tl ': r) tb -> Exp r tb  
lett = Let sin
 
sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc

prdAll :: Exp (t ': r) t' -> Exp r t' 
prdAll = mapVar (\(Suc x) -> x)
               
inc :: (forall t'. Var r t' -> Var r' t') -> Var (ta ': r) t -> Var (ta ': r') t
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)
 
mapVar :: forall r r' t. 
          (forall t'. Var r t' -> Var r' t') -> Exp r t -> Exp r' t
mapVar f ee = case ee of
  ConI i       -> ConI i
  ConB i       -> ConB i
  Var v        -> Var (f v)
  Abs eb       -> Abs (mf eb)  
  App ta ef ea -> App ta (m ef) (m ea)
  Cnd ec et ef -> Cnd (m ec)  (m et)  (m ef)
  Whl ec eb ei -> Whl (mf ec) (mf eb) (m ei)
  Tpl ef es    -> Tpl (m ef)  (m es)
  Fst ts e     -> Fst ts (m e)
  Snd tf e     -> Snd tf (m e)
  Ary el ef    -> Ary (m el) (mf ef)
  Len ta e     -> Len ta (m e)
  Ind ea ei    -> Ind (m ea) (m ei)
  Let tl el eb -> Let tl (m el) (mf eb)
  where
    m :: Exp r tt -> Exp r' tt
    m  = mapVar f
    
    mf :: Exp (ta ': r) tt -> Exp (ta ': r') tt
    mf = mapVar (inc f)

 