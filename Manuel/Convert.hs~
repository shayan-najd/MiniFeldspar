{-# LANGUAGE GADTs #-}

module Convert (convert) where

import Data.Typeable
import Data.Maybe

import qualified DeBruijn
import qualified HOAS


-- A layout of an environment an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t 
              => Layout env env' -> DeBruijn.Ix env t -> Layout env (env', t)

-- Yield the number of entries in an environment layout
--
size :: Layout env env' -> Int
size EmptyLayout        = 0
size (PushLayout lyt _) = size lyt + 1

-- Add an entry to a layout, incrementing all indices
--
inc :: Layout env env' -> Layout (env, t) env'
inc EmptyLayout         = EmptyLayout
inc (PushLayout lyt ix) = PushLayout (inc lyt) (DeBruijn.SuccIx ix)

-- Project the nth index out of an environment layout
--
prjIx :: Typeable t => Int -> Layout env env' -> DeBruijn.Ix env t
prjIx _ EmptyLayout       = error "Convert.prjIx: internal error"
prjIx 0 (PushLayout _ ix) = fromJust (gcast ix)
                              -- can't go wrong unless the library is wrong!
prjIx n (PushLayout l _)  = prjIx (n - 1) l

-- Convert a HOAS term -always a closed term- to a closed de Bruijn term
--
convert :: HOAS.Term t -> DeBruijn.Term () t
convert = cvt EmptyLayout
  where
    cvt :: Layout env env -> HOAS.Term t -> DeBruijn.Term env t
    cvt lyt (HOAS.Tag sz)      = DeBruijn.Var (prjIx (size lyt - sz - 1) lyt)
    cvt lyt (HOAS.Con v)       = DeBruijn.Con v
    cvt lyt (HOAS.Lam f)       = DeBruijn.Lam (cvt lyt' (f tag))
      where
        tag  = HOAS.Tag (size lyt)
        lyt' = inc lyt `PushLayout` DeBruijn.ZeroIx
    cvt lyt (HOAS.App fun arg) = DeBruijn.App (cvt lyt fun) (cvt lyt arg)
