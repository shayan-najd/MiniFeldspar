{-# LANGUAGE TemplateHaskell #-}
module Examples.Feldspar.Conversion where
{-
import qualified Language.Haskell.TH.Syntax              as TH 
import qualified Expression.Feldspar.ADTUntypedNamed     as FAUN
import qualified Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Expression.Feldspar.GADTChurch          as FGCD
import qualified Expression.Feldspar.GADTFirstOrder      as FGFO 
import qualified Expression.Feldspar.GADTHigherOrder     as FGHO
import qualified Expression.Feldspar.MiniWellScoped      as FMWS
import qualified Expression.Feldspar.GADTValue           as FGV

import qualified Examples.TemplateHaskell                as TH
import qualified Examples.Feldspar.ADTUntypedNamed       as FAUN
import qualified Examples.Feldspar.ADTUntypedDebruijn    as FAUD
import qualified Examples.Feldspar.GADTUntypedDebruijn   as FGUD
import qualified Examples.Feldspar.GADTChurch            as FGCD
import qualified Examples.Feldspar.GADTFirstOrder        as FGFO 
import qualified Examples.Feldspar.GADTHigherOrder       as FGHO
import qualified Examples.Feldspar.MiniWellScoped        as FMWS

import qualified Type.Feldspar                           as FTA
import qualified Singleton.TypeFeldspar                  as FTG 

import qualified Environment.ADT                         as EA
import qualified Environment.ADTTable                    as EM
import qualified Singleton.Environment                   as EG
 -}
-- import Conversion
import Conversion.Variable                     ()
import Conversion.Environment                  ()
import Conversion.Existential                  ()
import Conversion.Type.Feldspar                ()
import Conversion.Expression.TemplateHaskell   ()
import Conversion.Expression.Feldspar          ()

-- import Evaluation
import Evaluation.Feldspar.ADTUntypedNamed     ()
import Evaluation.Feldspar.ADTUntypedDebruijn  ()
import Evaluation.Feldspar.GADTUntypedDebruijn ()
import Evaluation.Feldspar.GADTFirstOrder      ()
import Evaluation.Feldspar.GADTHigherOrder     ()
import Evaluation.Feldspar.MiniWellScoped      ()
{-
import ErrorMonad
import Existential
import Singleton
-}
test :: Bool              
test = True
  
  -- isFour AUM.four && isFour' AUP.four && isFour ACP.four && isFourQ TH.four 
  --  && isFourHO GFO.four


{-
type Add    = FS.Arr FS.Int (FS.Arr FS.Int FS.Int)
type ExsExp = Exs2 GFO.Exp (G.Env G.Typ) G.Typ
type EnvAdd = Add ': '[]

typAddS :: FS.Typ
typAddS = FS.Int `FS.Arr` (FS.Int `FS.Arr` FS.Int)

typAddG :: G.Typ Add
typAddG = (G.Int `G.Arr` (G.Int `G.Arr` G.Int))

envAddVal :: Trm EnvAdd
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
isFourQ e = case (do e':: GFO.Exp EnvAdd FS.Int <- cnv (e, envAddTHN, envEmpTHN)
                     evl e' envAddVal) of
                Rgt i -> i == (4 :: Integer)
                Lft s -> error s   

isFourHO :: GFO.Exp EnvAdd FS.Int -> Bool
isFourHO e = case (do e' :: GHO.Exp EnvAdd FS.Int <- cnv e
                      evl e' envAddVal) of
               Rgt i -> i == (4 :: Integer)
               Lft s -> error s              
-}
