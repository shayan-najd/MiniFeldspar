module Expression.Feldspar.GADTTyped where
 
import Prelude   ()
import MyPrelude

import Variable.Scoped

import qualified Nat.ADT as NA

data Exp :: NA.Nat -> * -> * where 
  ConI :: Integer -> Exp n t 
  ConB :: Bool    -> Exp n t 
  Var  :: Var n   -> Exp n t 
  Abs  :: Exp (NA.Suc n) t -> Exp n t
  App  :: t -> Exp n t -> Exp n t -> Exp n t   
  Cnd  :: Exp n t -> Exp n t -> Exp n t -> Exp n t 
  Whl  :: Exp (NA.Suc n) t -> Exp (NA.Suc n) t -> Exp n t -> Exp n t
  Tpl  :: Exp n t -> Exp n t -> Exp n t 
  Fst  :: t -> Exp n t -> Exp n t
  Snd  :: t -> Exp n t -> Exp n t
  Ary  :: Exp n t -> Exp (NA.Suc n) t -> Exp n t  
  Len  :: t -> Exp n t -> Exp n t  
  Ind  :: Exp n t -> Exp n t -> Exp n t 
  Let  :: t -> Exp n t -> Exp (NA.Suc n) t -> Exp n t 

deriving instance Eq t   => Eq   (Exp n t) 
deriving instance Show t => Show (Exp n t)
deriving instance Functor        (Exp n)
deriving instance Foldable       (Exp n)
deriving instance Traversable    (Exp n)
                    
sucAll :: Exp n t -> Exp (NA.Suc n) t    
sucAll = mapVar Suc

prdAll :: Exp (NA.Suc n) t -> Exp n t
prdAll = mapVar prd
 
mapVar :: (Var n -> Var n') -> Exp n t -> Exp n' t
mapVar f ebb = case ebb of  
  ConI i       -> ConI i
  ConB b       -> ConB b
  Var v        -> Var (f v)
  Abs eb       -> Abs (mf eb)  
  App t  ef ea -> App t (m ef) (m ea)    
  Cnd ec et ef -> Cnd (m ec) (m et) (m ef)
  Whl ec eb ei -> Whl (mf ec) (mf eb) (m ei)
  Tpl ef es    -> Tpl (m ef) (m es)
  Fst t  e     -> Fst t (m e )
  Snd t  e     -> Snd t (m e )                      
  Ary el ef    -> Ary (m el) (mf ef)
  Len t  e     -> Len t (m e )                         
  Ind ea ei    -> Ind (m ea) (m ei)                         
  Let t  el eb -> Let t (m el) (mf eb) 
  where                    
    m  = mapVar f
    mf = mapVar (inc f)
        
sbs :: Exp n ta -> Var n -> Exp n ta -> Exp n ta  
sbs ebb v eaa = case ebb of  
  ConI i         -> ConI i
  ConB b         -> ConB b
  Var x 
    | x == v     -> eaa              
    | otherwise  -> ebb            
  Abs eb         -> Abs (sf eb)  
  App t ef ea    -> App t (s ef) (s ea)    
  Cnd ec et ef   -> Cnd (s ec) (s et) (s ef)
  Whl ec eb ei   -> Whl (sf ec) (sf eb) (s ei)
  Tpl ef es      -> Tpl (s ef) (s es)
  Fst t  e       -> Fst t (s e )
  Snd t  e       -> Snd t (s e )                      
  Ary el ef      -> Ary (s el) (sf ef)
  Len t  e       -> Len t (s e )                         
  Ind ea ei      -> Ind (s ea) (s ei)                         
  Let t  el eb   -> Let t (s el) (sf eb) 
  where
    s  e = sbs e v eaa 
    sf e = sbs e (Suc v) (sucAll eaa)

fre :: Exp (NA.Suc n) t -> [Var (NA.Suc n)]
fre = flip fre' Zro

fre' :: forall n t. Exp n t -> Var n -> [Var n] 
fre' ee v = case ee of
  ConI _       -> [] 
  ConB _       -> [] 
  Var x 
   | x >= v    -> [x] 
   | otherwise -> []                      
  Abs eb       -> ff eb
  App _  ef ea -> f  ef ++ f  ea 
  Cnd ec et ef -> f  ec ++ f  et ++ f ef
  Whl ec eb ei -> ff ec ++ ff eb ++ f ei
  Tpl ef es    -> f  ef ++ f  es
  Fst _  e     -> f  e
  Snd _  e     -> f  e                       
  Ary el ef    -> f  el ++ ff ef
  Len _  e     -> f  e  
  Ind ea ei    -> f  ea ++ f  ei
  Let _  el eb -> f  el ++ ff eb
  where
    f  e = fre' e v
    
    ff :: Exp (NA.Suc n) t -> [Var n]
    ff e = fmap prd (fre' e (Suc v))