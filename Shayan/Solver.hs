module Solver where

import Type.ADTWithMetavariable
import Control.Monad.State (State,get,put,modify,runState)
import ErrorMonad

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

-- Applying a list of substitions for metavariables
appTtas :: [(Int , Typ)] -> Typ -> Typ
appTtas ttas t = foldl (\ ta (i , t') -> appT i t' ta) t ttas 

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
