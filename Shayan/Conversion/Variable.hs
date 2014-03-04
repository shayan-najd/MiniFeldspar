module Conversion.Variable where

import qualified Data.Nat              as A
import qualified Variable              as G
import qualified Environment.ADT       as A
import qualified Singleton.Nat         as GN
import qualified Singleton.Environment as G
import qualified Data.Fin          as F
import Existential
import Conversion
import Conversion.Environment ()
import Conversion.Nat         ()
 
type ExsVar tf = Exs2 G.Var (G.Env tf) tf
type ExsFin    = Exs1 F.Nat GN.Nat
  
instance Cnv a (ExsSin b) => Cnv (A.Nat , A.Env a) (Exs2 G.Var (G.Env b) b) where
 cnv (A.Zro   , t : r) = do ExsSin r' <- cnv r 
                            ExsSin t' <- cnv t  
                            return (Exs2 G.Zro (t' `G.Ext` r') t')
 cnv (A.Suc x , t : r) = do Exs2 x' r' tr <- cnv (x , r)
                            ExsSin t'     <- cnv t
                            return (Exs2 (G.Suc x') (t' `G.Ext` r') tr)
 cnv (_       , [])    = fail "Impossible!"  
 
instance Cnv (A.Nat , GN.Nat n)  (F.Nat n) where
  cnv (A.Zro   , GN.Suc  _) = return F.Zro
  cnv (A.Suc n , GN.Suc n') = F.Suc <$> cnv (n , n')
  cnv (_       , _)         = fail "Impossible!"  
 
