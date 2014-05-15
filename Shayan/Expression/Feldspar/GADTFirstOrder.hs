module Expression.Feldspar.GADTFirstOrder where
 
import Prelude ()
import MyPrelude

import Variable.Typed

import Singleton

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

data Exp :: [TFA.Typ] -> TFA.Typ -> * where 
  ConI :: Integer  -> Exp r TFA.Int 
  ConB :: Bool     -> Exp r TFA.Bol
  ConF :: Float    -> Exp r TFA.Flt
  Var  :: Var r t  -> Exp r t  
  Abs  :: Exp (ta ': r) tb -> Exp r (TFA.Arr ta tb) 
  App  :: HasSin TFG.Typ ta => 
          Exp r (TFA.Arr ta tb) -> Exp r ta -> Exp r tb     
  Cnd  :: Exp r TFA.Bol -> Exp r t -> Exp r t -> Exp r t 
  Whl  :: Exp (t ': r) TFA.Bol -> Exp (t ': r) t -> Exp r t -> Exp r t  
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (TFA.Tpl tf ts) 
  Fst  :: HasSin TFG.Typ ts => Exp r (TFA.Tpl tf ts) -> Exp r tf 
  Snd  :: HasSin TFG.Typ tf => Exp r (TFA.Tpl tf ts) -> Exp r ts 
  Ary  :: Exp r TFA.Int -> Exp (TFA.Int ': r) t -> Exp r (TFA.Ary t)
  Len  :: HasSin TFG.Typ ta => Exp r (TFA.Ary ta) -> Exp r TFA.Int 
  Ind  :: Exp r (TFA.Ary ta) -> Exp r TFA.Int -> Exp r ta 
  Let  :: HasSin TFG.Typ tl => Exp r tl -> Exp (tl ': r) tb -> Exp r tb  
  Cmx  :: Exp r TFA.Flt -> Exp r TFA.Flt -> Exp r TFA.Cmx
  
sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc

prdAll :: Exp (t ': r) t' -> Exp r t' 
prdAll = mapVar (\(Suc x) -> x)
                
mapVar :: forall r r' t. 
          (forall t'. Var r t' -> Var r' t') -> Exp r t -> Exp r' t
mapVar f ee = case ee of
  ConI i       -> ConI i
  ConB i       -> ConB i
  ConF i       -> ConF i
  Var v        -> Var (f v)
  Abs eb       -> Abs (mf eb)  
  App ef ea    -> App (m ef)  (m ea)
  Cnd ec et ef -> Cnd (m ec)  (m et)  (m ef)
  Whl ec eb ei -> Whl (mf ec) (mf eb) (m ei)
  Tpl ef es    -> Tpl (m ef)  (m es)
  Fst e        -> Fst (m e)
  Snd e        -> Snd (m e)
  Ary el ef    -> Ary (m el)  (mf ef)
  Len e        -> Len (m e)
  Ind ea ei    -> Ind (m ea)  (m ei)
  Let el eb    -> Let (m el)  (mf eb)
  Cmx er ei    -> Cmx (m er)  (m ei)
  where
    m :: Exp r tt -> Exp r' tt
    m  = mapVar f
    
    mf :: Exp (ta ': r) tt -> Exp (ta ': r') tt
    mf = mapVar (inc f)

{- 
sbs :: forall r t t'.Exp r t' -> Var r t -> Exp r t -> Exp r t'      
sbs ebb v eaa = case ebb of
  ConI i       -> ConI i
  ConB i       -> ConB i
  Var x        
    | x == v     -> eaa              
    | otherwise  -> ebb  
  Abs eb       -> Abs (mf eb)  
  App ef ea    -> App (m ef)  (m ea)
  Cnd ec et ef -> Cnd (m ec)  (m et)  (m ef)
  Whl ec eb ei -> Whl (mf ec) (mf eb) (m ei)
  Tpl ef es    -> Tpl (m ef)  (m es)
  Fst e        -> Fst (m e)
  Snd e        -> Snd (m e)
  Ary el ef    -> Ary (m el)  (mf ef)
  Len e        -> Len (m e)
  Ind ea ei    -> Ind (m ea)  (m ei)
  Let el eb    -> Let (m el)  (mf eb)
  where
    m :: forall t'. Exp r t' -> Exp r t' 
    m  e = sbs e v eaa 
    
    mf :: forall t' tt. Exp (tt ': r) t' -> Exp (tt ': r) t' 
    mf e = sbs e (Suc v) (sucAll eaa) 
-}