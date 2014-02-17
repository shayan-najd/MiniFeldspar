{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
module Examples.Feldspar.Conversion where

import Language.Haskell.TH.Syntax
import qualified Expression.Feldspar.ADTUntypedDebruijn as AUM
import qualified Expression.Feldspar.GADTFirstOrder        as GFO 
import qualified Expression.Feldspar.GADTHigherOrder       as GHO

import qualified Type.Feldspar.ADTSimple  as FS
import qualified Singleton.TypeFeldspar   as G

import qualified Value.Feldspar.GADT as V
import Conversion
import Conversion.Expression.Feldspar ()
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Environment ()
import Conversion.Existential ()
import Conversion.Expression.TemplateHaskell ()
import qualified Environment.ADT  as A
import qualified Environment.ADTTable  as AT
import qualified Environment.GADT as G
import Evaluation.Feldspar.GADTFirstOrder()
import Evaluation.Feldspar.GADTHigherOrder()
import qualified Examples.Feldspar.ADTUntypedDebruijn  as AUM
import qualified Examples.Feldspar.ADTUntypedNamed  as AUP
import qualified Examples.Feldspar.ADTChurch   as ACP
import qualified Examples.Feldspar.GADTFirstOrder         as GFO
import qualified Examples.TemplateHaskell                 as TH
import Evaluation
import SingletonEquality
import ErrorMonad
import Existential
import Singleton.Environment ()

type ExsExp = Exs2 GFO.Exp (G.Env G.Typ) G.Typ
type EnvAdd = (Integer -> Integer -> Integer , ())

typAddS :: FS.Typ
typAddS = FS.Int `FS.Arr` (FS.Int `FS.Arr` FS.Int)

typAddG :: G.Typ (Integer -> Integer -> Integer)
typAddG = (G.Int `G.Arr` (G.Int `G.Arr` G.Int))

envAddVal :: EnvAdd
envAddVal = (V.addV , ())

envAddA :: A.Env FS.Typ
envAddA = [typAddS] 

envAddG :: G.Env G.Typ EnvAdd
envAddG =  G.Ext typAddG G.Emp

envAddTHN :: AT.Env Name FS.Typ
envAddTHN = [( '(+) , typAddS)] 

envAddStr :: AT.Env String FS.Typ
envAddStr = [("add" , typAddS)]                   

envEmpTHN  :: AT.Env Name AUM.Exp
envEmpTHN = [] 

envEmpStr  :: AT.Env String AUM.Exp
envEmpStr = [] 

isFour :: Cnv (e , A.Env FS.Typ) ExsExp => e -> Bool
isFour e  = case (do Exs2 e' r' G.Int :: ExsExp <- cnv (e, envAddA)
                     Rfl <- eqlSin r' envAddG                    
                     evl e' envAddVal) of
              Rgt i -> i == (4 :: Integer)
              Lft s -> error s   

isFour' :: forall ef. (Cnv (ef String, AT.Env String FS.Typ 
                           , AT.Env String AUM.Exp) ExsExp) => 
           ef String -> Bool
isFour' e = case (do Exs2 e' r' G.Int :: ExsExp <- cnv (e, envAddStr, envEmpStr)
                     Rfl <- eqlSin r' envAddG
                     evl e' envAddVal) of
              Rgt i -> i == (4 :: Integer)
              Lft s -> error s   
 
isFourQ :: Q (TExp Integer) -> Bool
isFourQ e = case (do e':: GFO.Exp EnvAdd Integer <- cnv (e, envAddTHN, envEmpTHN)
                     evl e' envAddVal) of
                Rgt i -> i == (4 :: Integer)
                Lft s -> error s   

isFourHO :: GFO.Exp EnvAdd Integer -> Bool
isFourHO e = case (do e' :: GHO.Exp EnvAdd Integer <- cnv e
                      evl e' envAddVal) of
               Rgt i -> i == (4 :: Integer)
               Lft s -> error s              

test :: Bool              
test = isFour AUM.four && isFour' AUP.four && isFour ACP.four && isFourQ TH.four 
    && isFourHO GFO.four