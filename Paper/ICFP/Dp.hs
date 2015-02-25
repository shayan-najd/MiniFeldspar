{-# LANGUAGE DataKinds,GADTs,TypeOperators,TypeFamilies #-}

import Data.Array
import Data.Complex

type Arr a = Array Int a

data Exp g a where
  LitB     :: Bool     -> Exp g Bool
  LitI     :: Int      -> Exp g Int
  LitF     :: Float    -> Exp g Float
  If       :: Exp g Bool -> Exp g a -> Exp g a -> Exp g a
  Whl      :: (Exp g a -> Exp g Bool) -> (Exp g a -> Exp g a) ->
              Exp g a  -> Exp g a
  Tpl      :: Exp g a -> Exp g b -> Exp g (a , b)
  Fst      :: Type b =>
              Exp g (a , b)-> Exp g a
  Snd      :: Type a =>
              Exp g (a , b)-> Exp g b
  Arr      :: Exp g Int -> (Exp g Int -> Exp g a) -> Exp g (Arr a)
  ArrLen   :: Type a =>
              Exp g (Arr a) -> Exp g Int
  ArrInd   :: Exp g (Arr a) -> Exp g Int -> Exp g a
  Variable :: String -> Exp g a  -- dummy constructor used for translation

  -- fully applied primitives, subsuming (in a type-safe manner)
  -- PrimN for any 0 <= N
  AppFull  :: Type a =>
              Var g a -> List g (ArgumentsOf a) -> Exp g (OutputOf a)

  Let      :: Type a =>
              Exp g a -> (Exp g a -> Exp g b) -> Exp g b

  -- Complex numbers are used in FFT
  Cmx      :: Exp g Float -> Exp g Float -> Exp g (Complex Float)

  -- Tags are used for CSE
  Tag      :: String -> Exp g a -> Exp g a
  Add      :: Exp g a -> Exp g a -> Exp g a
  Mul      :: Exp g a -> Exp g a -> Exp g a

-- an indexed list, whose elements are of the form "Exp g a"
data List g a where
  Emp :: List g '[]
  Ext :: Exp g a -> List g d -> List g (a ': d)

-- A class returning the term representation of type "a"
class Type a where
  -- ...

-- standard definition of variables to extract elements from
-- List type
data Var g a where
  Zro :: Var (a ': g) a
  Suc :: Var g a -> Var (b ': g) a

-- A simple type function to return the final / output type of
-- the given type when it is fully applied
type family OutputOf (a :: *) :: * where
  OutputOf (a -> b) = OutputOf b
  OutputOf a        = a

-- A simple type function to return a list containing the type of
-- arguments of the given type
type family ArgumentsOf (a :: *) :: [*] where
  ArgumentsOf (a -> b) = a ': ArgumentsOf b
  ArgumentsOf a        = '[]

-- For example, assuming Dp has only two primitives:
--   not :: Bool -> Bool, and and :: Bool -> Bool -> Bool.
-- Then Dp is defiend as the following. In QFeldspar,
-- thanks to the five benchmark programs, the list of primitives
-- is quite large.
type Dp a = Exp [Bool -> Bool , Bool -> Bool -> Bool] a
