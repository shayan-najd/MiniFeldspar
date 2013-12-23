{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs
           , ScopedTypeVariables #-}
module Expression.Conversion where

import qualified Expression.ADTUntypedUnscoped as UU
import qualified Expression.ADTUntyped as U
import qualified Expression.ADTChurchMonomorphic as ACM
import qualified Expression.ADTChurchPolymorphic as ACP
import qualified Expression.ADTExplicitPolymorphic as E
import qualified Expression.GADT as G
import qualified Expression.Existential as W

import qualified Type.ADTSimple as AS
import qualified Type.ADTWithMetavariable as AM
import qualified Type.GADT as G
import qualified Type.Existential as W
import Type.Conversion ()

import qualified Variable.ADT as A
import qualified Variable.Existential as W
import Variable.Conversion ()

import qualified Environment.ADT as A
import qualified Environment.GADT as G
import qualified Environment.Existential as W
import Environment.Conversion ()

import qualified ADTChurchPolymorphic as ACP
import qualified ADTExplicitPolymorphic as E

import Conversion
import Solver
import EqualityProof
import ErrorMonad

import Control.Applicative ((<$>),(<*>),pure)
import Data.Traversable(traverse)
import Control.Monad.State(runState)

instance Cnv (U.Exp , A.Env AS.Typ)  W.Exp where  
  cnv (e , r) = do e'   :: ACP.Exp AM.Typ <- cnv e
                   e''  :: ACP.Exp AS.Typ <- traverse cnv e'
                   e''' :: W.Exp          <- cnv (e'' , r)
                   return e'''

instance Cnv U.Exp (ACP.Exp AM.Typ) where
  cnv e = do let (e' , (i , cs)) = runState 
                  (do ee <- cnvExpUToACPAM e
                      _  <- ACP.chk ee []
                      return ee) (0 , [])  
             mTs <- slv (AM.Mta `map` [0 .. (i-1)]) cs
             let ttas = zip [0..] mTs -- substituitions
             return (appTtas ttas `fmap` e') 

-- Conversion from the untyped lambda calculus to the explicitly typed 
-- simply-typed lambda calculus (church style) where every explicit type 
-- annotaion is set as a fresh metavariable
cnvExpUToACPAM :: U.Exp -> InfM (ACP.Exp AM.Typ)
cnvExpUToACPAM (U.Con i)     = pure (ACP.Con i)
cnvExpUToACPAM (U.Var v)     = pure (ACP.Var v)
cnvExpUToACPAM (U.Abs eb)    = ACP.Abs <$> newMT   <*> cnvExpUToACPAM eb
cnvExpUToACPAM (U.App ef ea) = ACP.App <$> cnvExpUToACPAM ef 
                               <*> cnvExpUToACPAM ea
cnvExpUToACPAM (U.Add ef ea) = ACP.Add <$> cnvExpUToACPAM ef 
                               <*> cnvExpUToACPAM ea         

instance Cnv U.Exp (E.Exp AM.Typ) where
  cnv e = do let (e' , (i , cs)) = runState 
                  (do ee <- cnvExpUToEAM e
                      _  <- E.chk ee []
                      return ee) (0 , [])  
             mTs <- slv (AM.Mta `map` [0 .. (i-1)]) cs
             let ttas = zip [0..] mTs -- substituitions
             return (appTtas ttas `fmap` e') 

-- Conversion from the untyped lambda calculus to the explicitly typed 
-- simply-typed lambda calculus where every explicit type annotaion is
-- set as a fresh metavariable
cnvExpUToEAM :: U.Exp -> InfM (E.Exp AM.Typ)
cnvExpUToEAM (U.Con i)     = E.Con <$> newMT <*> pure i
cnvExpUToEAM (U.Var v)     = E.Var <$> newMT <*> pure v
cnvExpUToEAM (U.Abs eb)    = E.Abs <$> newMT <*> cnvExpUToEAM eb
cnvExpUToEAM (U.App ef ea) = E.App <$> newMT <*> cnvExpUToEAM ef 
                               <*> cnvExpUToEAM ea
cnvExpUToEAM (U.Add ef ea) = E.Add <$> newMT <*> cnvExpUToEAM ef 
                               <*> cnvExpUToEAM ea

instance Eq a => Cnv (UU.Exp a , [(a , U.Exp)]) U.Exp where
  cnv (e , rf) = cnvExpUUToU e [] rf 

cnvExpUUToU :: Eq a => UU.Exp a -> [(a , A.Nat)] -> [(a , U.Exp)] -> ErrM U.Exp 
cnvExpUUToU (UU.Con i)     _  _  = return (U.Con i)
cnvExpUUToU (UU.Var s)     rb rf = case (A.getTbl s rb , A.getTbl s rf) of
  (Just x  , _)                 -> return (U.Var x)
  (Nothing , Just e)            -> return e
  _                             -> fail "Scope Error!"
cnvExpUUToU (UU.Abs s  eb) rb rf = do eb' <- cnvExpUUToU eb 
                                             ((s , A.Zro) : sucAll rb) rf
                                      return (U.Abs eb')       
cnvExpUUToU (UU.App ef ea) rb rf = do ef' <- cnvExpUUToU ef rb rf 
                                      ea' <- cnvExpUUToU ea rb rf 
                                      return (U.App ef' ea')
cnvExpUUToU (UU.Add el er) rb rf = do el' <- cnvExpUUToU el rb rf 
                                      er' <- cnvExpUUToU er rb rf 
                                      return (U.Add el' er')     

sucAll :: [(a , A.Nat)] -> [(a , A.Nat)]
sucAll = map (\ (x , y) -> (x , A.Suc y))

instance Cnv (ACM.Exp , A.Env AS.Typ) W.Exp where
  cnv (ACM.Con i     , r) = do W.Env r' <- cnv r
                               return (W.Exp (G.Con i) r' G.Int)
  cnv (ACM.Var x     , r) = do W.Var x' r' t' <- cnv (x , r)
                               return (W.Exp (G.Var x') r' t')
  cnv (ACM.Abs ta eb , r) = do W.Typ ta' <- cnv ta
                               W.Exp eb' (ta'' `G.Ext` r') tb <- cnv(eb , ta : r)
                               Rfl <- eqlTyp ta' ta''
                               return (W.Exp (G.Abs ta' eb') r' (G.Arr ta' tb))
  cnv (ACM.App ef ea , r) = do W.Exp ef' rf (G.Arr ta tb) <- cnv (ef , r)
                               W.Exp ea' ra ta'           <- cnv (ea , r)
                               Rfl <- eqlEnv rf ra
                               Rfl <- eqlTyp ta ta'
                               return (W.Exp (G.App ef' ea') rf tb)
  cnv (ACM.Add el er , r) = do W.Exp el' rl G.Int <- cnv (el , r)
                               W.Exp er' rr G.Int <- cnv (er , r)
                               Rfl <- eqlEnv rl rr
                               return (W.Exp (G.Add el' er') rl G.Int)

instance Cnv (ACP.Exp AS.Typ , A.Env AS.Typ) W.Exp where  
  cnv (ACP.Con i     , r) = do W.Env r' <- cnv r
                               return (W.Exp (G.Con i) r' G.Int)
  cnv (ACP.Var x     , r) = do W.Var x' r' t' <- cnv (x , r)
                               return (W.Exp (G.Var x') r' t')
  cnv (ACP.Abs ta eb , r) = do W.Typ ta' <- cnv ta
                               W.Exp eb' (ta'' `G.Ext` r') tb <- cnv(eb , ta : r)
                               Rfl <- eqlTyp ta' ta''
                               return (W.Exp (G.Abs ta' eb') r' (G.Arr ta' tb))
  cnv (ACP.App ef ea , r) = do W.Exp ef' rf (G.Arr ta tb) <- cnv (ef , r)
                               W.Exp ea' ra ta'           <- cnv (ea , r)
                               Rfl <- eqlEnv rf ra
                               Rfl <- eqlTyp ta ta'
                               return (W.Exp (G.App ef' ea') rf tb)
  cnv (ACP.Add el er , r) = do W.Exp el' rl G.Int <- cnv (el , r)
                               W.Exp er' rr G.Int <- cnv (er , r)
                               Rfl <- eqlEnv rl rr
                               return (W.Exp (G.Add el' er') rl G.Int)


instance Cnv (E.Exp AS.Typ , A.Env AS.Typ) W.Exp where
  cnv (E.Con _ i     , r) = do W.Env r' <- cnv r
                               return (W.Exp (G.Con i) r' G.Int)
  cnv (E.Var _ x     , r) = do W.Var x' r' t' <- cnv (x , r)
                               return (W.Exp (G.Var x') r' t')
  cnv (E.Abs t eb    , r) = do let (ta `AS.Arr` _) = t
                               W.Typ ta' <- cnv ta
                               W.Exp eb' (ta'' `G.Ext` r') tb <- cnv(eb , ta : r)
                               Rfl <- eqlTyp ta' ta''
                               return (W.Exp (G.Abs ta' eb') r' (G.Arr ta' tb))
  cnv (E.App _ ef ea , r) = do W.Exp ef' rf (G.Arr ta tb) <- cnv (ef , r)
                               W.Exp ea' ra ta'           <- cnv (ea , r)
                               Rfl <- eqlEnv rf ra
                               Rfl <- eqlTyp ta ta'
                               return (W.Exp (G.App ef' ea') rf tb)
  cnv (E.Add _ el er , r) = do W.Exp el' rl G.Int <- cnv (el , r)
                               W.Exp er' rr G.Int <- cnv (er , r)
                               Rfl <- eqlEnv rl rr
                               return (W.Exp (G.Add el' er') rl G.Int)