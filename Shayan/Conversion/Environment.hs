module Conversion.Environment where

import qualified Environment.ADT        as A
import qualified Environment.ADTTable   as AT
import qualified Singleton.Environment  as G
import qualified Data.Vector            as V

import qualified Type.Feldspar          as FAS
import qualified Singleton.TypeFeldspar as FG

import Conversion.Type.Feldspar ()

import Conversion
import Existential
import Singleton
import Singleton.Environment (Len)
import Prelude hiding (mapM)
import Data.Traversable(mapM)

instance Cnv a (ExsSin b) => Cnv (A.Env a) (ExsSin (G.Env b)) where
  cnv []      = return (ExsSin G.Emp)
  cnv (t : r) = do ExsSin t' <- cnv t
                   ExsSin r' <- cnv r
                   return (ExsSin (t' `G.Ext` r'))
 
instance Cnv a b => Cnv (A.Env a) (A.Env b)  where
  cnv = mapM cnv
 
instance (Cnv a b , n ~ n') => Cnv (V.Vec n a) (V.Vec n' b) where  
  cnv = mapM cnv
  
instance Cnv (V.Vec n t) (A.Env t) where
  cnv V.Nil        = pure []
  cnv (x V.::: xs) = (x :) <$> cnv xs
    
instance (n ~ Len r) => Cnv (G.Env FG.Typ r) (V.Vec n FAS.Typ) where
  cnv G.Emp        = return V.Nil
  cnv (G.Ext x xs) = (V.:::) <$> cnv x <*> cnv xs
  

instance (EqlSin tf, Trm r ~ r' ) => 
         Cnv (G.Env tf r , A.Env (ExsTrm tf)) r' where 
  cnv (G.Emp        , [])                  = return ()
  cnv (t `G.Ext` ts , (ExsTrm x tv) : vs)  = do ts' <- cnv (ts , vs)
                                                Rfl <- eqlSin t tv 
                                                return (x,ts')
  cnv (_            , _)                   = fail "Scope Error!"  
 
cnvEnvAMAS :: Cnv a b => AT.Env x a -> ErrM (AT.Env x b)
cnvEnvAMAS = mapM (\(x , y) -> do y' <- cnv y
                                  return (x , y'))
