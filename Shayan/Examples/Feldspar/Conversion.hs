{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
module Examples.Feldspar.Conversion where
-- import qualified Expression.Existential as W

import Language.Haskell.TH.Syntax
import qualified Expression.Feldspar.ADTUntypedMonomorphic as AUM
import qualified Expression.Feldspar.GADT as G 
import qualified Type.Feldspar.ADTSimple  as FS
import qualified Type.Feldspar.GADT       as G

import qualified Value.Feldspar.GADT as V
import Conversion
import Conversion.Expression.Feldspar ()
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Environment ()
import Conversion.Existential ()
import Conversion.Expression.TemplateHaskell ()
import qualified Environment.ADT  as A
import qualified Environment.ADTTable  as AT
import qualified Environment.GADT as G
import Evaluation.Feldspar.GADT()
import qualified Examples.Feldspar.ADTUntypedMonomorphic  as AUM
import qualified Examples.Feldspar.ADTUntypedPolymorphic  as AUP
import qualified Examples.Feldspar.ADTChurchPolymorphic   as ACP
import qualified Examples.TemplateHaskell             as TH
import Evaluation
import SingletonEquality
import ErrorMonad
import Existential
import Singleton.Environment ()

type ExsExp = Exs2 G.Exp (G.Env G.Typ) G.Typ


isFour :: Cnv (e , A.Env FS.Typ) ExsExp => e -> Bool
isFour e  = case (do Exs2 e' r' G.Int 
                             :: ExsExp <- cnv (e , [FS.Int `FS.Arr` 
                                                    (FS.Int `FS.Arr` FS.Int)])
                     Rfl <- eqlSin r' (G.Ext (G.Int `G.Arr` 
                                                    (G.Int `G.Arr` G.Int))
                                                    G.Emp)                    
                     evl e' (V.addV, ())) of
              Rgt i -> i == (4 :: Integer)
              Lft s -> error s   

isFour' :: forall ef. (Cnv (ef String, AT.Env String FS.Typ 
                           , AT.Env String AUM.Exp) ExsExp) => 
           ef String -> Bool
isFour' e  = case (do Exs2 e' r' G.Int 
                             :: ExsExp <- cnv (e , [("add", FS.Int `FS.Arr` 
                                                    (FS.Int `FS.Arr` FS.Int))]
                                                 , [] :: AT.Env String AUM.Exp )
                      Rfl <- eqlSin r' (G.Ext (G.Int `G.Arr` 
                                                    (G.Int `G.Arr` G.Int))
                                                    G.Emp)
                      evl e' (V.addV, ())) of
               Rgt i -> i == (4 :: Integer)
               Lft s -> error s   
 
isFourQ :: Q (TExp Integer) -> Bool
isFourQ e  = case (do e':: G.Exp (Integer -> Integer -> Integer , ())  Integer 
                           <- cnv (e 
                                  , [( '(+)
                                     , FS.Int `FS.Arr` 
                                       (FS.Int `FS.Arr` FS.Int))] 
                                  , [] :: AT.Env Name AUM.Exp)
                      evl e' (V.addV , ())) of
                Rgt i -> i == (4 :: Integer)
                Lft s -> error s   

test :: Bool              
test = isFour AUM.four && isFour' AUP.four && isFour ACP.four && isFourQ TH.four