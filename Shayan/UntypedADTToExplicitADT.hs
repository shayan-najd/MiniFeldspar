{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,StandaloneDeriving #-}
module UntypedADTToExplicitADT where

import ErrorMonad
import Control.Monad.State (State,get,put,modify,runState)
import qualified ExplicitADT as E
import qualified UntypedADT as U
import qualified ADT as A

import Control.Applicative ((<$>),(<*>),pure)

-- Types with metavariables
data Typ =    
    Mta Int
  | Int   
  | Arr Typ Typ
    deriving Eq

-- Type equality constraint
infixr 6 :~:
data EqlC = Typ :~: Typ 

-- A monad for type inference carrying, as state, an integer for generating 
-- fresh names and a list of type equality constraints collected 
type InfM a = State (Int,[EqlC]) a

-- Generating fresh metavariables using fresh names
newMT :: InfM Typ
newMT = do (i , x) <- get
           put (i + 1 , x)
           return (Mta i)         

-- Adding a type equality constraint to the state
addC  :: EqlC -> InfM ()
addC c = modify (\ (i , cs) -> (i , c : cs))

-- Conversion from the untyped lambda calculus to the explicitly typed 
-- simply-typed lambda calculus where every explicit type annotaion is
-- set as a fresh metavariable
eExp :: U.Exp -> InfM (E.Exp Typ)
eExp (U.Con i)     = E.Con <$> newMT <*> pure i
eExp (U.Var v)     = E.Var <$> newMT <*> pure v
eExp (U.Abs eb)    = E.Abs <$> newMT <*> eExp eb
eExp (U.App ef ea) = E.App <$> newMT <*> eExp ef <*> eExp ea
eExp (U.Add ef ea) = E.Add <$> newMT <*> eExp ef <*> eExp ea         
                     
-- Collecting metavariables in a typ
mtas :: Typ -> [Typ]
mtas (Mta i)       = [Mta i]
mtas Int           = []
mtas (tl `Arr` tr) = mtas tl ++ mtas tr

-- Subtitution of a metavariable ([i := t]) in a type 
appT :: Int -> Typ -> Typ -> Typ
appT _ _ Int                 = Int
appT i t (t1 `Arr` t2)       = appT i t t1 `Arr` appT i t t2 
appT i t (Mta j) | j == i    = t
                 | otherwise = Mta j           

-- Subtitution of a metavariable ([i := t]) in a type equality constraint
appC :: Int -> Typ -> EqlC -> EqlC
appC i t (t1 :~: t2) = appT i t t1 :~: appT i t t2

-- Subtitution of a metavariable ([i := t]) in a list of types 
appTs :: Int -> Typ -> [Typ] -> [Typ]
appTs i t = map (appT i t)

-- Subtitution of a metavariable ([i := t]) in a list of constraints
appCs :: Int -> Typ -> [EqlC] -> [EqlC]
appCs i t = map (appC i t)

-- Setting the checker to collect constraints wherever types are unified
instance E.Chk (State (Int,[EqlC])) Typ where
  eql t  t' = addC (t :~: t') 
  eqlInt t  = E.eql t Int 
  eqlArr t  = do t1 <- newMT
                 t2 <- newMT                 
                 addC (t :~: t1 `Arr` t2)
                 return (t1 , t2)
                                        
-- Constraint Solving (Herbrand style constraint solving)
slv :: [Typ] -> [EqlC] -> ErrM [Typ]
slv mem []                                   
  = return mem
slv mem ((t :~: t') : cs) | t == t'  
  = slv mem cs
slv mem ((t1 `Arr` t2 :~: t1' `Arr` t2') : cs) 
  = slv mem ((t1 :~: t1') : (t2 :~: t2') : cs)
slv mem ((Mta i :~: t) : cs) | Mta i `notElem` mtas t
  = slv (appTs i t mem) (appCs i t cs)
slv mem ((t :~: Mta i) : cs) | Mta i `notElem` mtas t   
  = slv (appTs i t mem) (appCs i t cs)
slv _   ((_ :~: _ ) : _) 
  = fail "Type Error!" 

-- Type inference, returing an explicitly typed expression                       
inf :: U.Exp -> ErrM (E.Exp Typ)
inf e = do 
  let (e' , (i , cs)) = runState (do ee <- eExp e
                                     _  <- E.chk ee []
                                     return ee) (0 , [])  
  mTs <- slv (Mta <$> [0 .. (i-1)]) cs
  return ((\ (Mta k) -> mTs !! k) `fmap` e')                       
 
-- Extraction of top-level type
getTyp :: E.Exp Typ -> Typ 
getTyp (E.Con t _)   = t
getTyp (E.Var t _)   = t
getTyp (E.Abs t _)   = t
getTyp (E.App t _ _) = t
getTyp (E.Add t _ _) = t

tstDbl :: Bool
tstDbl = getTyp `fmap` (inf U.dbl) == Rgt (Int `Arr` Int)

tstCompose :: Bool
tstCompose = case getTyp `fmap` (inf U.compose) of 
   Rgt ((t0 `Arr` t1) `Arr` ((t2 `Arr` t3) `Arr` (t4 `Arr` t5))) 
     | t0 == t3 && t1 == t5 && t2 == t4 -> True
   _                                    -> False                               
       
tstFour :: Bool
tstFour = getTyp `fmap` (inf U.four) == Rgt Int

-- A non-example, a type-incorrect expression
nonEx :: U.Exp
nonEx = U.Abs (U.App (U.Var A.Zro) (U.Var A.Zro))

tstNonEx :: Bool
tstNonEx =  case inf nonEx of 
  Lft _ -> True
  _     -> False

test :: Bool
test = tstDbl && tstCompose && tstFour && tstNonEx

instance Show Typ where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 
  show (Mta n)                    = "a" ++ show n 
