{-# LANGUAGE GADTs #-}

module HOAS where

import Data.Typeable
import Text.Show.Functions

import qualified DeBruijn


-- Lambda terms using Haskell functions to represent functionals
--
-- * We don't care about exotic terms here, and hence don't pull the recursive
--   term structure out.
-- * The `Typeable' contexts and the `Tag' variant are in preparation for
--   being able to convert to a de Bruijn representation.
--
data Exp t where
  Tag :: Typeable t      -- for conversion to de Bruijn
      => Int -> Exp t    
  Con :: (Typeable t, Show t)
      => t -> Exp t
  Lam :: (Typeable ta, Typeable tb, Show ta, Show tb)
      => (Exp ta -> Exp tb) -> Exp (ta -> tb)
  App :: (Typeable ta, Typeable tb, Show ta, Show tb)
      => Exp (ta -> tb) -> Exp ta -> Exp tb

-- A term interpreter for closed terms
--
intp :: Show t => Exp t -> t
intp (Tag ix)      = error "HOAS.intp: Tag is only for conversion"
intp (Con v)       = v
intp (Lam fun)     = intp . fun . Con
intp (App fun arg) = (intp fun) (intp arg)
