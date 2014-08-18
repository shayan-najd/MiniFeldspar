module Test where

import MyPrelude

import qualified Examples.Feldspar.Simple.ADTUntypedNamed     as FAUN
import qualified Examples.Feldspar.Simple.ADTUntypedDebruijn  as FAUD
import qualified Examples.Feldspar.Simple.GADTUntypedDebruijn as FGUD
import qualified Examples.Feldspar.Simple.GADTTyped           as FGTD
import qualified Examples.Feldspar.Simple.GADTFirstOrder      as FGFO
import qualified Examples.Feldspar.Simple.GADTHigherOrder     as FGHO
import qualified Examples.Feldspar.Simple.MiniWellScoped      as FMWS
import qualified Examples.Feldspar.Simple.Conversion          as FCNV

import Expression.Feldspar.Conversions.Evaluation.ADTValue ()

main :: IO ()
main = print (if FAUN.test  && FAUD.test  && FGUD.test &&
                 FGTD.test  && FGFO.test  && FGHO.test &&
                 FMWS.test  && FCNV.test
              then "Pass!"
              else "Fail!")

-- Todo:
-- * Check for exotic terms
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
-- * Add sqrt and memorize (for Float) to Preludes that do not have it
-- * Write the code required for memorize
-- * Add support for partial type annotation *in* the quoted language
-- * Use macros for polymorphic datatypes
-- * Generate polymorphic datatypes
-- * Check all external imports are only via MyPrelude
-- * Shift Whole Pipline to the compile time!
-- * Use let in FFT and CRC
-- * Use eta