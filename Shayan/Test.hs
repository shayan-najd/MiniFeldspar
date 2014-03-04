module Test where
 
import qualified Examples.STLC.ADTUntypedDebruijn      as SAUM
-- import qualified Examples.STLC.GADTUntypedDebruijn as SGUM
import qualified Examples.STLC.ADTUntypedNamed         as SAUP
import qualified Examples.STLC.ADTChurch               as SACP
import qualified Examples.STLC.ADTExplicit             as SAEP
import qualified Examples.STLC.GADTFirstOrder          as SGFO
import qualified Examples.STLC.GADTHigherOrder         as SGHO
-- import qualified Examples.STLC.MiniWellScoped         as SMWS
import qualified Examples.STLC.Conversion              as SCNV

import qualified Examples.Feldspar.ADTUntypedDebruijn  as FAUM
import qualified Examples.Feldspar.GADTUntypedDebruijn as FGUM
import qualified Examples.Feldspar.ADTUntypedNamed     as FAUP
import qualified Examples.Feldspar.ADTChurch           as FACP
-- import qualified Examples.Feldspar.ADTExplicit         as FAEP
import qualified Examples.Feldspar.GADTFirstOrder      as FGFO
import qualified Examples.Feldspar.GADTHigherOrder     as FGHO
import qualified Examples.Feldspar.Mini                as FMin
import qualified Examples.Feldspar.MiniWellScoped      as FMWS
import qualified Examples.Feldspar.Conversion          as FCNV

import PrettyPrinter.Expression.STLC.ADTUntypedDebruijn  ()
-- import PrettyPrinter.Expression.STLC.GADTUntypedDebruijn ()
import PrettyPrinter.Expression.STLC.ADTUntypedNamed     ()
import PrettyPrinter.Expression.STLC.ADTChurch           ()  
import PrettyPrinter.Expression.STLC.ADTExplicit         ()  
import PrettyPrinter.Expression.STLC.GADTFirstOrder      () 
-- import PrettyPrinter.Expression.STLC.GADTHigherOrder     () 
-- import PrettyPrinter.Expression.STLC.MiniWellScoped      ()

import PrettyPrinter.Expression.Feldspar.ADTUntypedDebruijn  ()
-- import PrettyPrinter.Expression.Feldspar.GADTUntypedDebruijn ()
import PrettyPrinter.Expression.Feldspar.ADTUntypedNamed     ()
import PrettyPrinter.Expression.Feldspar.ADTChurch           ()  
-- import PrettyPrinter.Expression.Feldspar.ADTExplicit         ()
import PrettyPrinter.Expression.Feldspar.GADTFirstOrder      () 
-- import PrettyPrinter.Expression.Feldspar.GADTHigherOrder     () 
-- import PrettyPrinter.Expression.Feldspar.Mini                ()
-- import PrettyPrinter.Expression.Feldspar.MiniWellScoped      ()

import PrettyPrinter.Type.STLC     ()
import PrettyPrinter.Type.Feldspar ()
import PrettyPrinter.Type.Mini     ()

import PrettyPrinter.Variable ()
import PrettyPrinter.Nat      ()

import Normalization.Feldspar.ADTChurch ()
import Conversion.Expression.Feldspar.Normalization ()

main :: IO ()
main = print (if SAUP.test && SAUM.test && -- FGUM.test &&
                 SACP.test && SAEP.test &&  
                 SGFO.test && SGHO.test &&
                 --  SMin.test && SMWS.test && 
                 SCNV.test && 
                 
                 FAUP.test && FAUM.test && FGUM.test &&
                 FACP.test && -- FAEP.test &&  
                 FGFO.test && FGHO.test &&
                 FMin.test && FMWS.test && 
                 FCNV.test 
              then "Pass!"
              else "Fail!")