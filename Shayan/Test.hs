{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Test where
 
import qualified Examples.STLC.ADTUntypedMonomorphic  as SAUM
import qualified Examples.STLC.ADTUntypedPolymorphic  as SAUP
import qualified Examples.STLC.ADTChurchPolymorphic   as SACP
import qualified Examples.STLC.ADTExplicitPolymorphic as SAEP
import qualified Examples.STLC.GADTFirstOrder         as SGFO
import qualified Examples.STLC.GADTHigherOrder        as SGHO
import qualified Examples.STLC.Conversion             as SCNV

import qualified Examples.Feldspar.ADTUntypedMonomorphic  as FAUM
import qualified Examples.Feldspar.ADTUntypedPolymorphic  as FAUP
import qualified Examples.Feldspar.ADTChurchPolymorphic   as FACP
import qualified Examples.Feldspar.GADTFirstOrder         as FGFO
import qualified Examples.Feldspar.GADTHigherOrder        as FGHO
import qualified Examples.Feldspar.Conversion             as FCNV

main :: IO ()
main = print (if SAUM.test && SACP.test && SAEP.test && 
                 SAUP.test && SGFO.test && SGHO.test && SCNV.test && 
                 FAUM.test && FACP.test &&   
                 FAUP.test && FGFO.test && FGHO.test && FCNV.test 
              then "Pass!"
              else "Fail!")