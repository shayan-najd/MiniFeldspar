{-# LANGUAGE GADTs #-}

module Convert (convert) where

import Data.Typeable
import Data.Maybe

import qualified DeBruijn as F
import qualified HOAS     as H


-- A layout of an environment an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Lay r r' where
  Emp :: Lay r ()
  Ext  :: Typeable t => 
          Lay r r' -> F.Var r t -> Lay r (r' , t)

-- Yield the number of entries in an environment layout
--
size :: Lay r r' -> Int
size Emp         = 0
size (Ext lyt _) = size lyt + 1

-- Add an entry to a layout, incrementing all indices
--
inc :: Lay r r' -> Lay (r, t) r'
inc Emp          = Emp 
inc (Ext lyt ix) = Ext (inc lyt) (F.Suc ix)

-- Project the nth index out of an environment layout
--
prjIx :: Typeable t => Int -> Lay r r' -> F.Var r t
prjIx _ Emp        = error "Convert.prjIx: internal error"
prjIx 0 (Ext _ ix) = fromJust (gcast ix)
                              -- can't go wrong unless the library is wrong!
prjIx n (Ext l _)  = prjIx (n - 1) l

-- Convert a HOAS term -always a closed term- to a closed de Bruijn term
--
convert :: H.Exp t -> F.Exp () t
convert = cvt Emp 
  where
    cvt :: Lay r r -> H.Exp t -> F.Exp r t
    cvt lyt (H.Tag sz)      = F.Var (prjIx (size lyt - sz - 1) lyt)
    cvt lyt (H.Con v)       = F.Con v
    cvt lyt (H.Lam f)       = F.Lam (cvt lyt' (f tag))
      where
        tag  = H.Tag (size lyt)
        lyt' = inc lyt `Ext` F.Zro
    cvt lyt (H.App fun arg) = F.App (cvt lyt fun) (cvt lyt arg)
