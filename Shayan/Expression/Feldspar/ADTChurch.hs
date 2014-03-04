module Expression.Feldspar.ADTChurch where
 
import Data.Traversable
import Data.Foldable
import Data.Nat 

type Var = Nat

data Exp t = ConI Integer 
           | ConB Bool 
           | Var Var  
           | Abs t (Exp t)
           | App (Exp t) (Exp t)  
           | Cnd (Exp t) (Exp t) (Exp t) 
           | Whl t (Exp t) (Exp t) (Exp t) 
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
  ConI i         -> ConI i
  ConB b         -> ConB b
  Var x 
    | x == v     -> eaa              
    | otherwise  -> ebb            
  App ef ea      -> App (s ef) (s ea)    
  Abs t eb       -> Abs t (sf eb)
  Cnd ec et ef   -> Cnd (s ec) (s et) (s ef)
  Whl t ec eb ei -> Whl t (sf ec) (sf eb) (s ei)
  Tpl ef es      -> Tpl (s ef) (s es)
  Fst e          -> Fst (s e )
  Snd e          -> Snd (s e )                      
  Ary el ef      -> Ary (s el) (sf ef)
  Len e          -> Len (s e )                         
  Ind ea ei      -> Ind (s ea) (s ei)                         
  Let el eb      -> Let (s el) (sf eb) 
  where
    s  e = sbs e v eaa 
    sf e = sbs e (Suc v) (sucAll eaa)

sucAll :: Exp t -> Exp t    
sucAll = mapVar Suc

inc :: (Var -> Var) -> Var -> Var
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)

mapVar :: (Var -> Var) -> Exp t -> Exp t
mapVar f ebb = case ebb of  
  ConI i         -> ConI i
  ConB b         -> ConB b
  Var v          -> Var (f v)
  App ef ea      -> App (m ef) (m ea)    
  Abs t eb       -> Abs t (mf eb)
  Cnd ec et ef   -> Cnd (m ec) (m et) (m ef)
  Whl t ec eb ei -> Whl t (mf ec) (mf eb) (m ei)
  Tpl ef es      -> Tpl (m ef) (m es)
  Fst e          -> Fst (m e )
  Snd e          -> Snd (m e )                      
  Ary el ef      -> Ary (m el) (mf ef)
  Len e          -> Len (m e )                         
  Ind ea ei      -> Ind (m ea) (m ei)                         
  Let el eb      -> Let (m el) (mf eb) 
  where                    
    m  = mapVar f
    mf = mapVar (inc f)
  
fre :: Exp t -> [Var]
fre = flip fre' Zro

fre' :: Exp t -> Var -> [Var] 
fre' ee v = case ee of
  ConI _              -> [] 
  ConB _              -> [] 
  Var x 
    | x >= v          -> [x] 
    | otherwise       -> []                      
  App ef ea           -> f ef ++ f ea 
  Abs _  eb           -> ff eb
  Cnd ec et ef        -> f ec ++ f et ++ f ef
  Whl _ ec eb ei      -> ff ec ++ ff eb ++ f ei
  Tpl ef es           -> f ef ++ f es
  Fst e               -> f e
  Snd e               -> f e                       
  Ary el ef           -> f el ++ ff ef
  Len e               -> f e  
  Ind ea ei           -> f ea ++ f ei
  Let el eb           -> f el ++ ff eb
  where
    f  e = fre' e v
    ff e = fre' e (Suc v)
                     