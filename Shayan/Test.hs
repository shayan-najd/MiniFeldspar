module Test where

import Prelude ()
import MyPrelude

import qualified Examples.Feldspar.ADTUntypedNamed         as FAUN
import qualified Examples.Feldspar.ADTUntypedDebruijn      as FAUD
import qualified Examples.Feldspar.GADTUntypedDebruijn     as FGUD
import qualified Examples.Feldspar.GADTTyped               as FGTD
import qualified Examples.Feldspar.GADTFirstOrder          as FGFO
import qualified Examples.Feldspar.GADTHigherOrder         as FGHO
import qualified Examples.Feldspar.MiniWellScoped          as FMWS
import qualified Examples.Feldspar.Conversion              as FCNV
-- import qualified Examples.Feldspar.Prelude.TemplateHaskellSolution1A as FPTHS1A
-- import qualified Examples.Feldspar.Prelude.TemplateHaskellSolution1B as FPTHS1B
-- import qualified Examples.Feldspar.Prelude.TemplateHaskellSolution2A as FPTHS2A
-- import qualified Examples.Feldspar.Prelude.TemplateHaskellSolution2B as FPTHS2B
import qualified Examples.Feldspar.Prelude.MiniWellScoped  as FPMWS ()
import qualified Examples.Feldspar.Prelude.GADTHigherOrder as FPGHO
import qualified Examples.Feldspar.Prelude.GADTFirstOrder  as FPGFO
import Conversion.Expression.Feldspar.Evaluation.ADTValue () 
import Normalization.Feldspar.GADTTyped ()

main :: IO ()
main = print (if FAUN.test  && FAUD.test  && FGUD.test &&
                 FGTD.test  && FGFO.test  && FGHO.test &&
                 FMWS.test  && FCNV.test  &&
                 FPGFO.test && FPGHO.test -- &&
                 --  FPMWS.test && 
                 --    FPTHS1A.test && FPTHS1B.test && FPTHS2A.test && FPTHS2B.test 
              then "Pass!"
              else "Fail!")
-- Todo:       
-- * Check for exotic terms
-- * Let construct seems hacky
-- * Weakening of HOAS
--    + getting rid of Tmp
-- * Bidirectional translations
-- * Using Type classes to do lifting and colifting for ADTValue and GADTValue
--   to reuse Vanilla Prelude
-- * check for all exhaustive partterns and transform them
-- * Conversion of FGHO (x :-> y) ~> (FMWS x -> FMWS y)
-- * Free Fusion for Church / Ahman's Containers
-- * Supporting F
-- * Scope Proofing Quotations (e.g. Sam's misunderstanding) [EncodingTypes.txt] 
-- * Support for Syntactic Suggar in Quotations (e.g. use TH-Desugar)