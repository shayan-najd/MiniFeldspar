module Inference where

import Prelude ()
import MyPrelude

import qualified TypeChecking as Chk
import Nat.ADT
import Type.Herbrand
import Solver 
import Conversion
import Conversion.Nat ()

mxm :: [Nat] -> Nat
mxm [] = Zro
mxm l  = maximum l

ind :: Traversable ef => 
       ef () -> ef Nat
ind = flip evalState Zro .  
      traverse (const (do i <- getState
                          put (Suc i)
                          return i))

typInf :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) => 
       ef () -> (Chk.Env ef) (Typ r) -> ErrM (ef (Typ r))       
typInf e r = let en = ind e
                 et = fmap Mta en
             in inf et r

maxMta :: Foldable f => f (Typ r) -> Nat  
maxMta = mxm . concat . fmap mtas . toList
 
inf :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) => 
       ef (Typ r) -> (Chk.Env ef) (Typ r) -> ErrM (ef (Typ r))
inf e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (i' , cs) = execState (Chk.chk e r) (succ mts , [])
             mTs <- slv (fmap Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs 
             return (fmap (appTtas ttas) e)
  
chk :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) => 
       ef (Typ r) -> (Chk.Env ef) (Typ r) -> ErrM (Typ r)
chk e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (t , (i' , cs)) = runState (Chk.chk e r) (mts , [])
             mTs <- slv (fmap Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs
             if length [() | Mta _ <- mTs] == 0
               then return (appTtas ttas t)
               else fail "Type Error!"   

typChk :: forall ef t. 
          (Chk.Chk ef , Traversable ef, Cnv (t , ()) (Typ (Chk.Cns ef))
          , Traversable (Chk.Env ef)
          , Cnv (Typ (Chk.Cns ef), ()) t)  => 
          ef t -> Chk.Env ef t-> ErrM t
typChk e r = do r' :: (Chk.Env ef) (Typ (Chk.Cns ef)) <- traverse 
                                                         (flip (curry cnv) ()) r
                e' :: ef  (Typ (Chk.Cns ef)) <- traverse (flip (curry cnv) ()) e
                t <- chk e' r'
                cnv (t , ())