module Conversion.Type.STLC where

import qualified Type.STLC as AS
import qualified Singleton.TypeSTLC as T
import qualified Type.Herbrand as H
import qualified Variable      as G
import Data.Vector
import Conversion
import Existential

---------------------------------------------------------------------------------
-- Conversion from AS.Typ
---------------------------------------------------------------------------------
instance Cnv AS.Typ (ExsSin T.Typ) where
  cnv AS.Int         = return (ExsSin T.Int)
  cnv (AS.Arr ta tr) = do ExsSin ta' <- cnv ta
                          ExsSin tr' <- cnv tr
                          return (ExsSin (T.Arr ta' tr'))

instance Cnv AS.Typ (H.Typ (H.EnvIntArr '[])) where
  cnv AS.Int         = pure (H.App G.Zro Nil)
  cnv (AS.Arr ta tr) = do ta' <- cnv ta
                          tr' <- cnv tr
                          return (H.App (G.Suc G.Zro) (ta' ::: tr' ::: Nil))
---------------------------------------------------------------------------------
-- Conversion from H.Typ
---------------------------------------------------------------------------------
instance Cnv (H.Typ (H.EnvIntArr '[])) AS.Typ where
  cnv (H.App G.Zro Nil)  = pure AS.Int
  cnv (H.App (G.Suc G.Zro) (ta ::: tr ::: Nil)) 
                         = do ta' <- cnv ta
                              tr' <- cnv tr
                              return (AS.Arr ta' tr')
  cnv _                  = fail "Type Error!"                   
