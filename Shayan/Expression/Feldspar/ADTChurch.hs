{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module Expression.Feldspar.ADTChurch where
 
import Data.Traversable
import Data.Foldable
import Variable.ADT

data Exp t = ConI Integer 
           | ConB Bool 
           | Var Var  
           | Abs t (Exp t)
           | App (Exp t) (Exp t)  
           | Cnd (Exp t) (Exp t) (Exp t) 
           | Whl (Exp t) (Exp t) (Exp t) 
           | Tpl (Exp t) (Exp t)
           | Fst (Exp t) 
           | Snd (Exp t)
           | Ary (Exp t) (Exp t)
           | Len (Exp t) 
           | Ind (Exp t) (Exp t) 
           | Let (Exp t) (Exp t) 
           deriving (Eq,Functor,Foldable,Traversable)
                    
sbs :: Exp ta -> Var -> Exp ta -> Exp ta 
sbs ebb v eaa = case ebb of  
  ConI i        -> ConI i
  ConB b        -> ConB b
  Var x 
    | x == v    -> eaa              
    | otherwise -> ebb            
  App ef ea     -> App (s ef) (s ea)    
  Abs t eb      -> Abs t (sbs eb (Suc v) (sucAll eaa))
  Cnd ec et ef  -> Cnd (s ec) (s et) (s ef)
  Tpl ef es     -> Tpl (s ef) (s es)
  Fst e         -> Fst (s e )
  Snd e         -> Snd (s e )                      
  Ary el ef     -> Ary (s el) (s ef)
  Len e         -> Len (s e )                         
  Ind ea ei     -> Ind (s ea) (s ei)                         
  Whl ec eb ei  -> Whl (s ec) (s eb) (s ei)
  Let el eb     -> Let (s el) (sbs eb (Suc v) (sucAll eaa)) 
  where
    s e = sbs e v eaa 

sucAll :: Exp t -> Exp t    
sucAll = mapVar Suc

inc :: (Var -> Var) -> Var -> Var
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)

mapVar :: (Var -> Var) -> Exp t -> Exp t
mapVar f ebb = case ebb of  
  ConI i        -> ConI i
  ConB b        -> ConB b
  Var v         -> Var (f v)
  App ef ea     -> App (mapVar f ef) (mapVar f ea)    
  Abs t eb      -> Abs t (mapVar (inc f) eb)
  Cnd ec et ef  -> Cnd (mapVar f ec) (mapVar f et) (mapVar f ef)
  Tpl ef es     -> Tpl (mapVar f ef) (mapVar f es)
  Fst e         -> Fst (mapVar f e )
  Snd e         -> Snd (mapVar f e )                      
  Ary el ef     -> Ary (mapVar f el) (mapVar f ef)
  Len e         -> Len (mapVar f e )                         
  Ind ea ei     -> Ind (mapVar f ea) (mapVar f ei)                         
  Whl ec eb ei  -> Whl (mapVar f ec) (mapVar f eb) (mapVar f ei)
  Let el eb     -> Let (mapVar f el) (mapVar (inc f) eb) 
                     
fre :: Exp t -> [Var]
fre = flip fre' Zro

fre' :: Exp t -> Var -> [Var] 
fre' ee v = case ee of
  ConI _              -> [] 
  ConB _              -> [] 
  Var x 
    | x >= v          -> [x] 
    | otherwise       -> []                      
  App ef ea           -> fre' ef v ++ fre' ea v 
  Abs _  eb           -> fre' eb (Suc v)
  Cnd ec et ef        -> fre' ec v ++ fre' et v ++ fre' ef v
  Tpl ef es           -> fre' ef v ++ fre' es v
  Fst e               -> fre' e  v
  Snd e               -> fre' e  v                      
  Ary el ef           -> fre' el v ++ fre' ef v
  Len e               -> fre' e  v 
  Ind ea ei           -> fre' ea v ++ fre' ei v
  Whl ec eb ei        -> fre' ec v ++ fre' eb v ++ fre' ei v
  Let el eb           -> fre' el v ++ fre' eb (Suc v)
                     