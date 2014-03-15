module Test where

import Prelude ()
import MyPrelude

import qualified Examples.Feldspar.ADTUntypedNamed     as FAUN
import qualified Examples.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified Examples.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Examples.Feldspar.GADTTyped           as FGTD
import qualified Examples.Feldspar.GADTFirstOrder      as FGFO
import qualified Examples.Feldspar.GADTHigherOrder     as FGHO
import qualified Examples.Feldspar.MiniWellScoped      as FMWS
import qualified Examples.Feldspar.Conversion          as FCNV
 
import Normalization.Feldspar.GADTTyped ()
import Conversion.Expression.Feldspar.Normalization ()

main :: IO ()
main = print (if FAUN.test && FAUD.test && FGUD.test &&
                 FGTD.test && FGFO.test && FGHO.test &&
                 FMWS.test && FCNV.test 
              then "Pass!"
              else "Fail!")