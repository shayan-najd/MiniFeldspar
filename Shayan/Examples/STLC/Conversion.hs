{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables #-}
module Examples.STLC.Conversion where
-- import qualified Expression.Existential as W

import Language.Haskell.TH.Syntax
import qualified Expression.STLC.ADTUntypedDebruijn as AUM
import qualified Expression.STLC.GADTFirstOrder as GFO 
import qualified Expression.STLC.GADTHigherOrder as GHO 
import qualified Type.STLC.ADTSimple  as AS
import qualified Singleton.TypeSTLC   as G

import Conversion
import Conversion.Expression.STLC ()
import Conversion.Expression.TemplateHaskell ()
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Environment ()

import qualified Environment.ADT  as A
import qualified Environment.ADTTable  as AT
import qualified Environment.GADT as G
import Evaluation.STLC.GADTFirstOrder  ()
import Evaluation.STLC.GADTHigherOrder ()
import qualified Examples.STLC.ADTUntypedDebruijn  as AUM
import qualified Examples.STLC.ADTUntypedNamed  as AUP
import qualified Examples.STLC.ADTChurch   as ACP
import qualified Examples.STLC.ADTExplicit as AEP
import qualified Examples.STLC.GADTFirstOrder         as GFO
import qualified Examples.TemplateHaskell             as TH
import Evaluation
import Singleton.Environment ()
import ErrorMonad
import Existential
type ExsExp = Exs2 GFO.Exp (G.Env G.Typ) G.Typ

isFour :: Cnv (e , A.Env AS.Typ) ExsExp => e -> Bool
isFour e  = case (do Exs2 e' G.Emp G.Int 
                             :: ExsExp <- cnv (e , [] :: A.Env AS.Typ)
                     evl e' ()) of
              Rgt i -> i == (4 :: Integer)
              Lft s -> error s   

isFour' :: forall x ef. (Eq x , Cnv (ef x, AT.Env x AS.Typ 
                                    , AT.Env x AUM.Exp) ExsExp) => 
           ef x -> Bool
isFour' e  = case (do Exs2 e' G.Emp G.Int 
                             :: ExsExp <- cnv (e , [] :: AT.Env x AS.Typ 
                                                 , [] :: AT.Env x AUM.Exp)
                      evl e' ()) of
               Rgt i -> i == (4 :: Integer)
               Lft s -> error s   

isFourQ :: Q (TExp Integer) -> Bool
isFourQ e  = case (do e':: GFO.Exp () Integer <- cnv (e 
                                                   , [] :: AT.Env Name AS.Typ 
                                                   , [] :: AT.Env Name AUM.Exp)
                      evl e' ()) of
                Rgt i -> i == (4 :: Integer)
                Lft s -> error s   

isFourHO :: GFO.Exp () Integer -> Bool
isFourHO e = case (do e' :: GHO.Exp () Integer <- cnv e 
                      evl e' ()) of
               Rgt i -> i == (4 :: Integer)
               Lft s -> error s              


test :: Bool              
test = isFour AUM.four && isFour' AUP.four &&  
       isFour ACP.four && isFour  AEP.four && isFourQ TH.four
    && isFourHO GFO.four