{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module ExplicitADTToGADT where

import qualified ADT as A
import qualified GADT as T
import qualified ExplicitADT as E
import qualified UntypedADTToExplicitADT as UE
import qualified ADTToGADT as AT 
import ADTToGADT  (Eql(..),eqlTyp,eqlEnv,Typ(..),Env(..),Var(..),Exp(..))
import Prelude hiding (exp)
import ErrorMonad
import Control.Monad((<=<))

 
-- Type Translation from ExplicitADTs to GADTs
typ :: UE.Typ -> ErrM Typ
typ = AT.typ <=< atyp
         
-- Type Translation from ExplicitADTs to ADTs
atyp :: UE.Typ -> ErrM A.Typ
atyp (UE.Mta _)     = fail "Type Error!" 
atyp UE.Int         = return A.Int
atyp (UE.Arr ta tr) = do ta' <- atyp ta
                         tr' <- atyp tr
                         return (ta' `A.Arr` tr')

-- Environment Translation from ExplicitADTs to GADTs
env ::  [UE.Typ]  -> ErrM Env
env = AT.env <=< aenv
 
aenv :: [UE.Typ] -> ErrM [A.Typ]
aenv = mapM atyp 

class GetArgTyp a where
  getArgTyp :: a -> ErrM A.Typ

instance GetArgTyp UE.Typ where
  getArgTyp (ta `UE.Arr` _) = atyp ta
  getArgTyp _               = fail "Type Error!"
  
instance GetArgTyp A.Typ where
  getArgTyp (ta `A.Arr` _) = return ta
  getArgTyp _              = fail "Type Error!"
  
-- Expression Translation from ADTs to GADTs
exp :: GetArgTyp a =>  E.Exp a -> [A.Typ] -> ErrM Exp
exp (E.Con _ i)     r = do Env r' <- AT.env r
                           return (Exp (T.Con i) r' T.Int)
exp (E.Var _ x)     r = do Var x' r' t' <- AT.var x r
                           return (Exp (T.Var x') r' t')
exp (E.Abs t eb)    r = do ta <- getArgTyp t
                           Typ ta' <- AT.typ ta
                           Exp eb' (ta'' `T.Ext` r') tb <- exp eb (ta : r)
                           Rfl <- eqlTyp ta' ta''
                           return (Exp (T.Abs ta' eb') r' (T.Arr ta' tb))
exp (E.App _ ef ea) r = do Exp ef' rf (T.Arr ta tb) <- exp ef r
                           Exp ea' ra ta'           <- exp ea r
                           Rfl <- eqlEnv rf ra
                           Rfl <- eqlTyp ta ta'
                           return (Exp (T.App ef' ea') rf tb)
exp (E.Add _ el er) r = do Exp el' rl T.Int <- exp el r
                           Exp er' rr T.Int <- exp er r
                           Rfl <- eqlEnv rl rr
                           return (Exp (T.Add el' er') rl T.Int)

evl :: GetArgTyp a => E.Exp a -> Int
evl m  = case (do Exp m' T.Emp T.Int <- exp m []
                  return (T.evl m' ())) of
           Rgt i -> i
           Lft s -> error s   
  
test :: Bool
test = evl E.four == 4

main :: Bool
main = A.test && T.test && E.test && test
