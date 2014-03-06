module Conversion.Expression.Feldspar.TypeWithnessing where

import qualified Expression.Feldspar.GADTTyped      as FGCP
import qualified Expression.Feldspar.GADTFirstOrder as FGFO
import qualified Singleton.TypeFeldspar             as FG
import qualified Type.Feldspar                      as FAS
import qualified Singleton.Environment              as G

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable      ()

import Existential

type ExsTyp = ExsSin FG.Typ
 
instance (r ~ r' , t ~ t' , n ~ G.Len r) =>
         Cnv (FGCP.Exp n FAS.Typ , G.Env FG.Typ r , FG.Typ t)  
             (FGFO.Exp r' t') where
  cnv (ee , r , t) = case (ee , t) of
    (FGCP.ConI i       , FG.Int)       -> FGFO.ConI <$> pure i
    (FGCP.ConB b       , FG.Bol)       -> FGFO.ConB <$> pure b
    (FGCP.Var x        , _)            -> FGFO.Var  <$> cnv (x , r , t)
    (FGCP.Abs eb       , FG.Arr ta tb) -> FGFO.Abs  <$> cnv (eb,G.Ext ta r,tb) 
    (FGCP.App ta ef ea , _)            -> do ExsSin ta' :: ExsTyp <- cnv ta
                                             FGFO.App <$> pure ta' 
                                               <*> cnv (ef , r , FG.Arr ta' t)
                                               <*> cnv (ea , r , ta')
    (FGCP.Cnd ec et ef , _)            -> FGFO.Cnd <$> cnv (ec , r , FG.Bol)     
                                          <*> cnv (et , r , t)
                                          <*> cnv (ef , r , t)
    (FGCP.Whl ec eb ei , _)            -> FGFO.Whl 
                                          <$> cnv (ec , G.Ext t r , FG.Bol)
                                          <*> cnv (eb , G.Ext t r , t)
                                          <*> cnv (ei , r , t)
    (FGCP.Tpl ef es    , FG.Tpl tf ts) -> FGFO.Tpl <$> cnv (ef , r , tf) 
                                                   <*> cnv (es , r , ts) 
    (FGCP.Fst ts e     , _)            -> do ExsSin ts' :: ExsTyp <- cnv ts
                                             FGFO.Fst <$> pure ts' 
                                               <*> cnv (e , r , FG.Tpl t ts') 
    (FGCP.Snd tf e     , _)            -> do ExsSin tf' :: ExsTyp <- cnv tf
                                             FGFO.Snd <$> pure tf' 
                                               <*> cnv (e , r , FG.Tpl tf' t)
    (FGCP.Ary el ef    , FG.Ary ta)    -> FGFO.Ary 
                                           <$> cnv (el , r , FG.Int) 
                                           <*> cnv (ef , G.Ext FG.Int r , ta)
    (FGCP.Len ta e     , FG.Int )      -> do ExsSin ta' :: ExsTyp <- cnv ta
                                             FGFO.Len <$> pure ta'
                                               <*> cnv (e , r , FG.Ary ta')
    (FGCP.Ind e  ei    , _)            -> FGFO.Ind <$> cnv (e  , r , FG.Ary t)
                                                   <*> cnv (ei , r , FG.Int)
    (FGCP.Let tl el eb , _)            -> do ExsSin tl' :: ExsTyp <- cnv tl 
                                             FGFO.Let <$> pure tl' 
                                              <*> cnv (el , r , tl')
                                              <*> cnv (eb , G.Ext tl' r , t) 
    (_                 , _)            -> fail "Type Error!"
 