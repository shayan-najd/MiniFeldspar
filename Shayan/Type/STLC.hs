{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Type.STLC where

import Control.Monad(unless)

-- Types
data Typ =
    Int
  | Arr Typ Typ
  deriving Eq

-- Equality between types
(===) :: Monad m => Typ -> Typ -> m ()
t1 === t2 = unless (t1 == t2) (fail "Type Error!" )

instance Show Typ where                  
  show Int                        = "Int"
  show (t1@(_  `Arr` _) `Arr` t2) = "(" ++ show t1 ++ ") -> " ++ show t2 
  show (t1  `Arr` t2)             = show t1 ++ " -> " ++ show t2 