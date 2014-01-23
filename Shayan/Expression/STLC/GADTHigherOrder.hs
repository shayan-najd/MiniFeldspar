{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Expression.STLC.GADTHigherOrder where
 
import Variable.GADT

-- Higher-Order GADT representation (Debruijn indices) of the simply-typed 
-- lambda calculus expressions with Integer constants and a built-in addition 
-- operator
data Exp r t where
  Var :: Var r t -> Exp r t
  Con :: Integer -> Exp r Integer
  Add :: Exp r Integer -> Exp r Integer -> Exp r Integer
  Abs :: (Exp r ta -> Exp r tb) -> Exp r (ta -> tb)
  App :: Exp r (ta -> tb) -> Exp r ta -> Exp r tb 