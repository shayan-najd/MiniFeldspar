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
import qualified Examples.Feldspar.Prelude.TemplateHaskell as FPTH
import qualified Examples.Feldspar.Prelude.MiniWellScoped  as FPMWS
import qualified Examples.Feldspar.Prelude.GADTHigherOrder as FPGHO
import qualified Examples.Feldspar.Prelude.GADTFirstOrder  as FPGFO
--import qualified Examples.Feldspar.IP.MiniWellScoped  as FIMWS
--import qualified Examples.Feldspar.IP.TemplateHaskell as FITH
import Conversion.Expression.Feldspar.Evaluation.ADTValue () 
import Normalization.Feldspar.GADTTyped ()

main :: IO ()
main = print (if FAUN.test  && FAUD.test  && FGUD.test &&
                 FGTD.test  && FGFO.test  && FGHO.test &&
                 FMWS.test  && FCNV.test  &&
                 FPGFO.test && FPGHO.test &&
                 FPMWS.test && FPTH.test                  
              then "Pass!"
              else "Fail!")