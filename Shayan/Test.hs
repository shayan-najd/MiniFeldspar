{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Test where
 
import qualified Examples.STLC.ADTUntypedMonomorphic  as SAUM
import qualified Examples.STLC.ADTUntypedPolymorphic  as SAUP
import qualified Examples.STLC.ADTChurchPolymorphic   as SACP
import qualified Examples.STLC.ADTExplicitPolymorphic as SAEP
import qualified Examples.STLC.GADT                   as SGDT
import qualified Examples.STLC.Conversion             as SCNV

import qualified Examples.Feldspar.ADTUntypedMonomorphic  as FAUM
import qualified Examples.Feldspar.ADTUntypedPolymorphic  as FAUP
import qualified Examples.Feldspar.ADTChurchPolymorphic   as FACP
import qualified Examples.Feldspar.GADT                   as FGDT
import qualified Examples.Feldspar.Conversion             as FCNV

main :: IO ()
main = print (if SAUM.test && SACP.test && SAEP.test && 
                 SAUP.test && SGDT.test && SCNV.test && 
                 FAUM.test && FACP.test &&   
                 FAUP.test && FGDT.test && FCNV.test 
              then "Pass!"
              else "Fail!")