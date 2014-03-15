module Conversion.Type.Feldspar () where

import Prelude ()
import MyPrelude 

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG
import qualified Type.Herbrand      as TH

import Variable.Typed     

import Environment.Scoped

import Conversion 

---------------------------------------------------------------------------------
--  Conversion from TFA.Typ
---------------------------------------------------------------------------------
instance Cnv (TFA.Typ) (ExsSin TFG.Typ) where  
  cnv TFA.Int         = return (ExsSin TFG.Int)
  cnv TFA.Bol         = return (ExsSin TFG.Bol)
  cnv (TFA.Arr ta tr) = do ExsSin ta' <- cnv ta
                           ExsSin tr' <- cnv tr
                           return (ExsSin (TFG.Arr ta' tr'))
  cnv (TFA.Tpl tf ts) = do ExsSin tf' <- cnv tf
                           ExsSin ts' <- cnv ts
                           return (ExsSin (TFG.Tpl tf' ts'))
  cnv (TFA.Ary t)     = do ExsSin t' <- cnv t
                           return (ExsSin (TFG.Ary t'))
                            
instance Cnv (TFA.Typ , ()) (TH.Typ (TH.EnvFld '[])) where
  cnv (th , ()) = let ?r = () in case th of 
    TFA.Int       -> pure TH.int
    TFA.Bol       -> pure TH.bol
    TFA.Arr ta tb -> TH.arr <$@> ta <*@> tb 
    TFA.Tpl tf ts -> TH.tpl <$@> tf <*@> ts                             
    TFA.Ary ta    -> TH.ary <$@> ta
 
---------------------------------------------------------------------------------
--  Conversion from TFG.Typ
---------------------------------------------------------------------------------
instance Cnv (TFG.Typ a , ()) TFA.Typ where
  cnv (tt , ()) = let ?r =  () in case tt of
    TFG.Int       -> pure TFA.Int
    TFG.Bol       -> pure TFA.Bol
    TFG.Arr ta tb -> TFA.Arr <$@> ta <*@> tb 
    TFG.Tpl tf ts -> TFA.Tpl <$@> tf <*@> ts
    TFG.Ary ta    -> TFA.Ary <$@> ta
---------------------------------------------------------------------------------
--  Conversion from TH.Typ
---------------------------------------------------------------------------------
instance Cnv (TH.Typ (TH.EnvFld '[]) , ()) TFA.Typ where
  cnv (th , ()) = let ?r = () in case th of 
    TH.App Zro _                  -> pure TFA.Int      
    TH.App (Suc Zro) 
      (Ext ta (Ext tb Emp))       -> TFA.Arr <$@> ta <*@> tb
    TH.App (Suc (Suc Zro)) _      -> pure TFA.Bol
    TH.App (Suc (Suc 
      (Suc Zro))) 
      (Ext tf (Ext ts Emp))       -> TFA.Tpl <$@> tf <*@> ts
    TH.App (Suc (Suc (Suc 
      (Suc Zro)))) 
      (Ext t Emp)                 -> TFA.Ary <$@> t    
    _                             -> fail "Type Error!"

instance ts ~ ts' => Cnv (TFG.Typ ts, a) (TFG.Typ ts') where
  cnv = pure . fst