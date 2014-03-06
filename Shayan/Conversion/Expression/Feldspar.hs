module Conversion.Expression.Feldspar () where

import qualified Language.Haskell.TH.Syntax              as TH
import qualified Expression.Feldspar.ADTUntypedNamed     as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Expression.Feldspar.GADTTyped           as FGCP
import qualified Expression.Feldspar.GADTFirstOrder      as FGFO
import qualified Expression.Feldspar.GADTHigherOrder     as FGHO
import qualified Expression.Feldspar.MiniWellScoped      as FMWS

import qualified Type.Feldspar           as FAS
import qualified Singleton.TypeFeldspar  as FG

import qualified Environment.ADT         as A
-- import qualified Environment.ADTTable    as AT
import qualified Singleton.Environment   as G
import Singleton.Environment  (Len)
import qualified Data.Vector             as V

-- import qualified Data.Fin                as F

import Conversion
import Conversion.Variable                             ()
import Conversion.Environment                          ()
import Conversion.Existential                          ()
import Conversion.Type.Feldspar                        ()
import Conversion.Expression.TemplateHaskell           ()
import Conversion.Expression.Feldspar.Unquoting        ()
import Conversion.Expression.Feldspar.NameResolution   ()
import Conversion.Expression.Feldspar.ScopeWithnessing ()
import Conversion.Expression.Feldspar.TypeInference    ()
import Conversion.Expression.Feldspar.TypeWithnessing  ()
import Conversion.Expression.Feldspar.Lifting          ()
import Conversion.Expression.Feldspar.Normalization    ()


-- import Existential
import Singleton

-- type ExsExp = Exs2 FGFO.Exp (G.Env FG.Typ) FG.Typ
-- type SinTyp = HasSin FG.Typ
-- type SinEnv = HasSin (G.Env FG.Typ)
  
---------------------------------------------------------------------------------
-- Conversion from TH.TExp
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , Trm t ~ tt) => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGHO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
                      
instance (r ~ r' , t ~ t' , n ~ Len r , Trm t ~ tt) => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGHO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGFO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)           
           
instance (r ~ r' , t ~ t' , n ~ Len r, Trm t ~ tt) => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGFO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGCP.Exp n FAS.Typ <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)           
           
instance (n ~ Len r, Trm t ~ tt)  => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGCP.Exp n FAS.Typ) 
         where           
  cnv (e , r , t , v) = do e' :: FGUD.Exp n <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
           
instance (n ~ Len r, Trm t ~ tt) => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGUD.Exp n) 
         where           
  cnv (e , r , t , v) = do e' :: FAUD.Exp <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
   
instance (n ~ Len r, Trm t ~ tt) => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             FAUD.Exp
         where           
  cnv (e , r , t , v) = do e' :: FAUP.Exp TH.Name <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
           
instance (n ~ Len r, Trm t ~ tt) => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FAUP.Exp TH.Name)
         where           
  cnv (e , r , t , v) = do e' :: TH.Exp <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)
                                            
instance (n ~ Len r, Trm t ~ tt) => 
         Cnv (TH.Q (TH.TExp tt) , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             TH.Exp
         where           
  cnv (e , _ , _ , _) = TH.runQ (TH.unTypeQ e)          

---------------------------------------------------------------------------------
-- Conversion from TH.Exp
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (TH.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGHO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
                      
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (TH.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGHO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGFO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)           
           
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (TH.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGFO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGCP.Exp n FAS.Typ <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)           
           
instance (n ~ Len r)  => 
         Cnv (TH.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGCP.Exp n FAS.Typ) 
         where           
  cnv (e , r , t , v) = do e' :: FGUD.Exp n <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
           
instance (n ~ Len r) => 
         Cnv (TH.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGUD.Exp n) 
         where           
  cnv (e , r , t , v) = do e' :: FAUD.Exp <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
   
instance (n ~ Len r) => 
         Cnv (TH.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             FAUD.Exp
         where           
  cnv (e , r , t , v) = do e' :: FAUP.Exp TH.Name <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
           
instance (n ~ Len r) => 
         Cnv (TH.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FAUP.Exp TH.Name)
         where           
  cnv (e , _ , _ , _) = cnv e                
                                            
---------------------------------------------------------------------------------
-- Conversion from FAUP
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (FAUP.Exp TH.Name , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGHO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
                      
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (FAUP.Exp TH.Name , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGHO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGFO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)           
           
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (FAUP.Exp TH.Name , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGFO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGCP.Exp n FAS.Typ <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)           
           
instance (n ~ Len r)  => 
         Cnv (FAUP.Exp TH.Name , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGCP.Exp n FAS.Typ) 
         where           
  cnv (e , r , t , v) = do e' :: FGUD.Exp n <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
           
instance (n ~ Len r) => 
         Cnv (FAUP.Exp TH.Name , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGUD.Exp n) 
         where           
  cnv (e , r , t , v) = do e' :: FAUD.Exp <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)                      
   
instance (n ~ Len r) => 
         Cnv (FAUP.Exp TH.Name , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             FAUD.Exp
         where           
  cnv (e , _ , _ , vs) = do vs' :: A.Env TH.Name <- cnv vs
                            cnv (e , vs')                  
                                            
---------------------------------------------------------------------------------
-- Conversion from FAUD
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FAUD.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGHO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
                      
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FAUD.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGHO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGFO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)           
           
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (FAUD.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGFO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGCP.Exp n FAS.Typ <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)           
           
instance n ~ Len r => 
         Cnv (FAUD.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGCP.Exp n FAS.Typ) 
         where           
  cnv (e , r , t , v) = do e' :: FGUD.Exp n <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)           
           
instance n ~ Len r => 
         Cnv (FAUD.Exp , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGUD.Exp n) 
         where           
  cnv (e , r , _ , _) = cnv (e , G.len r)

---------------------------------------------------------------------------------
-- Conversion from FGUD
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FGUD.Exp n , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGHO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
                      
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FGUD.Exp n , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGHO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGFO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)           
           
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (FGUD.Exp n , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGFO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGCP.Exp n FAS.Typ <- cnv (e , r , t , v)
                           cnv (e' , r , t , v)           
           
instance (n ~ n' , n ~ Len r) => 
         Cnv (FGUD.Exp n , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGCP.Exp n' FAS.Typ) 
         where           
  cnv (e , r , _ , _) = do r' :: V.Vec n FAS.Typ <- cnv r           
                           cnv (e , r')
           
---------------------------------------------------------------------------------
-- Conversion from FGCP
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FGCP.Exp n FAS.Typ , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGHO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
                      
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FGCP.Exp n FAS.Typ , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGHO.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGFO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
           
instance (r ~ r' , t ~ t' , n ~ Len r) => 
         Cnv (FGCP.Exp n FAS.Typ , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGFO.Exp r' t') 
         where
  cnv (e , r , t , _) = cnv (e , r , t)           
  
---------------------------------------------------------------------------------
-- Conversion from FGFO
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FGFO.Exp r t , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where
  cnv (e , r , t , v) = do e' :: FGHO.Exp r t <- cnv (e , r , t , v)        
                           cnv (e' , r , t , v)
           
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FGFO.Exp r t , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FGHO.Exp r' t') 
         where
  cnv (e , r , _ , _) = cnv (e , r)         
    
---------------------------------------------------------------------------------
-- Conversion from FGHO
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r) =>
         Cnv (FGHO.Exp r t , G.Env FG.Typ r , FG.Typ t , V.Vec n TH.Name) 
             (FMWS.Exp r' t') 
         where           
  cnv (e , r , t , _ ) = cnv (e , r , t)        

{-
---------------------------------------------------------------------------------
-- Conversion from FAUP
---------------------------------------------------------------------------------
instance (Eq x , SinEnv r , SinTyp t') => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUD.Exp) 
             (FGFO.Exp r t')  where
  cnv (e , rt , rf) = do e' :: FAUD.Exp <- cnv (e , rt , rf)
                         cnv (e', map snd rt)

instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUD.Exp) ExsExp  where
  cnv (e , rt , rf) = do e' :: FGCP.Exp FAS.Typ <- cnv (e  , rt , rf)
                         cnv (e' , map snd rt)                   

instance Eq x => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUD.Exp) 
             (FGCP.Exp FAS.Typ) where
  cnv (e , rt , rf) = do e' :: FAUD.Exp <- cnv (e , rt , rf)
                         cnv (e'  , map snd rt)                          
---------------------------------------------------------------------------------
-- Conversion from FAUD
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t') => 
         Cnv (FAUD.Exp , A.Env FAS.Typ) (FGFO.Exp r t')  where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)       
                   cnv e'

instance Cnv (FAUD.Exp, A.Env FAS.Typ) ExsExp where
  cnv (e , r) = do e' :: FGCP.Exp FAS.Typ <- cnv (e , r)
                   cnv (e' , r)
---------------------------------------------------------------------------------
-- Conversion from FGCP
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t') => 
         Cnv (FGCP.Exp FAS.Typ, A.Env FAS.Typ) (FGFO.Exp r t')  where
  cnv (e , r) = do e' :: ExsExp           <- cnv (e , r)       
                   cnv e' 
 -}