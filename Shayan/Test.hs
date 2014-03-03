module Test where
 
import qualified Examples.STLC.ADTUntypedDebruijn      as SAUM
import qualified Examples.STLC.ADTUntypedNamed         as SAUP
import qualified Examples.STLC.ADTChurch               as SACP
import qualified Examples.STLC.ADTExplicit             as SAEP
import qualified Examples.STLC.GADTFirstOrder          as SGFO
import qualified Examples.STLC.GADTHigherOrder         as SGHO
import qualified Examples.STLC.Conversion              as SCNV

import qualified Examples.Feldspar.ADTUntypedDebruijn  as FAUM
import qualified Examples.Feldspar.GADTUntypedDebruijn as FGUM
import qualified Examples.Feldspar.ADTUntypedNamed     as FAUP
import qualified Examples.Feldspar.ADTChurch           as FACP
import qualified Examples.Feldspar.GADTFirstOrder      as FGFO
import qualified Examples.Feldspar.GADTHigherOrder     as FGHO
import qualified Examples.Feldspar.Mini                as Mini
import qualified Examples.Feldspar.MiniWellScoped      as MiWS
import qualified Examples.Feldspar.Conversion          as FCNV

main :: IO ()
main = print (if SAUM.test && SACP.test && SAEP.test &&  
                 SAUP.test && SGFO.test && SGHO.test && SCNV.test && 
                 FAUM.test && FACP.test && FGUM.test && 
                 FAUP.test && FGFO.test && FGHO.test && FCNV.test && 
                 Mini.test && MiWS.test
              then "Pass!"
              else "Fail!")