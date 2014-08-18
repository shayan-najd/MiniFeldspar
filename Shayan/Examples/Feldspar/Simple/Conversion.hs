module Examples.Feldspar.Simple.Conversion where

import MyPrelude

import qualified Language.Haskell.TH.Syntax              as TH
import qualified Expression.Feldspar.ADTUntypedNamed     as FAUN
import qualified Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Expression.Feldspar.GADTTyped           as FGTD
import qualified Expression.Feldspar.GADTFirstOrder      as FGFO
import qualified Expression.Feldspar.GADTHigherOrder     as FGHO
import qualified Expression.Feldspar.MiniWellScoped      as FMWS
import qualified Expression.Feldspar.ADTValue            as FAV
import qualified Expression.Feldspar.GADTValue           as FGV

import qualified Examples.Feldspar.Simple.TemplateHaskell     as TH
import qualified Examples.Feldspar.Simple.ADTUntypedNamed     as FAUN
import qualified Examples.Feldspar.Simple.ADTUntypedDebruijn  as FAUD
import qualified Examples.Feldspar.Simple.GADTUntypedDebruijn as FGUD
import qualified Examples.Feldspar.Simple.GADTTyped           as FGTD
import qualified Examples.Feldspar.Simple.GADTFirstOrder      as FGFO
import qualified Examples.Feldspar.Simple.GADTHigherOrder     as FGHO
import qualified Examples.Feldspar.Simple.MiniWellScoped      as FMWS ()

import qualified Type.Feldspar.ADT                       as TFA
import qualified Type.Feldspar.GADT                      as TFG

import qualified Environment.Map                         as EM
import qualified Environment.Plain                       as EP
import qualified Environment.Scoped                      as ES
import qualified Environment.Typed                       as ET

import Normalization
import Normalization.Feldspar.GADTHigherOrder  ()

import Conversion
import Variable.Conversion                     ()
import Environment.Conversion                  ()
import Type.Feldspar.Conversion                ()
import Expression.TemplateHaskell.Conversion   ()
import Expression.Feldspar.Conversion          ()

import qualified Nat.ADT as NA

type One    = NA.Suc NA.Zro
type Add    = TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int)
type EnvAdd = Add ': '[]

typAddG :: TFG.Typ Add
typAddG = (TFG.Arr TFG.Int (TFG.Arr TFG.Int TFG.Int))

envAddTypG :: ET.Env TFG.Typ EnvAdd
envAddTypG =  ET.Ext typAddG ET.Emp

vec :: ES.Env One TH.Name
vec = ES.Ext '(+) ES.Emp

envAddValG :: ET.Env FGV.Exp EnvAdd
envAddValG = ET.Ext (FGV.Exp (+)
                       :: FGV.Exp (TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int)))
             ET.Emp

envAddValV :: ES.Env One FAV.Exp
envAddValV = ES.Ext FAV.addV ES.Emp

envAddValA :: EP.Env FAV.Exp
envAddValA = FAV.addV : []

envAddValM :: EM.Env TH.Name FAV.Exp
envAddValM = ('(+) , FAV.addV) : []

cnvFMWS :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
               (FGHO.Exp EnvAdd TFA.Int) => e -> Integer -> Bool
cnvFMWS e j = case (do e'   :: FGHO.Exp EnvAdd TFA.Int <- cnv (e , envAddTypG
                                                              , vec)
                       let e'' = nrm e'
                       e''' :: FMWS.Exp EnvAdd TFA.Int <- cnv (e'' , envAddTypG
                                                              ,vec)
                       curry cnv e''' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvFGHO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGHO.Exp EnvAdd TFA.Int) => e -> Integer -> Bool
cnvFGHO e j = case (do e' :: FGHO.Exp EnvAdd  TFA.Int <- cnv (e , envAddTypG,vec)
                       curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvFGFO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGFO.Exp EnvAdd TFA.Int) => e -> Integer -> Bool
cnvFGFO e j = case (do e' :: FGFO.Exp EnvAdd TFA.Int <- cnv (e , envAddTypG ,vec)
                       curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _             -> False

cnvFGTD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGTD.Exp One TFA.Typ) => e -> Integer -> Bool
cnvFGTD e j = case (do e' :: FGTD.Exp One TFA.Typ <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValV) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

cnvFGUD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGUD.Exp One) => e -> Integer -> Bool
cnvFGUD e j = case (do e' :: FGUD.Exp One <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValV) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

cnvFAUD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           FAUD.Exp => e -> Integer -> Bool
cnvFAUD e j = case (do e' :: FAUD.Exp <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValA) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

cnvFAUN :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FAUN.Exp TH.Name) => e -> Integer -> Bool
cnvFAUN e j = case (do e' :: FAUN.Exp TH.Name <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValM) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

test :: Bool
test = cnvFAUN TH.four   4 && cnvFAUN FAUN.four 4 &&

       cnvFAUD TH.four   4 && cnvFAUD FAUN.four 4 && cnvFAUD FAUD.four 4 &&

       cnvFGUD TH.four   4 && cnvFGUD FAUN.four 4 && cnvFGUD FAUD.four 4 &&
       cnvFGUD FGUD.four 4 &&

       cnvFGTD TH.four   4 && cnvFGTD FAUN.four 4 && cnvFGTD FAUD.four 4 &&
       cnvFGTD FGUD.four 4 && cnvFGTD FGTD.four 4 &&

       cnvFGFO TH.four   4 && cnvFGFO FAUN.four 4 && cnvFGFO FAUD.four 4 &&
       cnvFGFO FGUD.four 4 && cnvFGFO FGTD.four 4 && cnvFGFO FGFO.four 4 &&

       cnvFGHO TH.four   4 && cnvFGHO FAUN.four 4 && cnvFGHO FAUD.four 4 &&
       cnvFGHO FGUD.four 4 && cnvFGHO FGTD.four 4 && cnvFGHO FGFO.four 4 &&
       cnvFGHO FGHO.four 4 &&

       cnvFMWS TH.four   4 && cnvFMWS FAUN.four 4 && cnvFMWS FAUD.four 4 &&
       cnvFMWS FGUD.four 4 && cnvFMWS FGTD.four 4 && cnvFMWS FGFO.four 4 &&
       cnvFMWS FGHO.four 4 -- && cnvFMWS FWMS.four 4
