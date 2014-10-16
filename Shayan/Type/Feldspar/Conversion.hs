module Type.Feldspar.Conversion () where

import MyPrelude

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG
import qualified Type.Herbrand      as TH

import Conversion

---------------------------------------------------------------------------------
--  Conversion from TFA.Typ
---------------------------------------------------------------------------------
instance Cnv (TFA.Typ) (ExsSin TFG.Typ) where
  cnv TFA.Int         = return (ExsSin TFG.Int)
  cnv TFA.Bol         = return (ExsSin TFG.Bol)
  cnv TFA.Flt         = return (ExsSin TFG.Flt)

  cnv (TFA.Arr ta tr) = do ExsSin ta' <- cnv ta
                           ExsSin tr' <- cnv tr
                           return (ExsSin (TFG.Arr ta' tr'))
  cnv (TFA.Tpl tf ts) = do ExsSin tf' <- cnv tf
                           ExsSin ts' <- cnv ts
                           return (ExsSin (TFG.Tpl tf' ts'))
  cnv (TFA.Ary t)     = do ExsSin t' <- cnv t
                           return (ExsSin (TFG.Ary t'))
  cnv TFA.Cmx         = return (ExsSin TFG.Cmx)
  cnv (TFA.May t)     = do ExsSin t' <- cnv t
                           return (ExsSin (TFG.May t'))

instance Cnv (TFA.Typ , r) (TH.Typ (TH.EnvFld '[])) where
  cnv (th , r) = let ?r = r in case th of
    TFA.Int       -> pure TH.Int
    TFA.Bol       -> pure TH.Bol
    TFA.Flt       -> pure TH.Flt
    TFA.Arr ta tb -> TH.Arr <$@> ta <*@> tb
    TFA.Tpl tf ts -> TH.Tpl <$@> tf <*@> ts
    TFA.Ary ta    -> TH.Ary <$@> ta
    TFA.May ta    -> TH.May <$@> ta
    TFA.Cmx       -> pure TH.Cmx

---------------------------------------------------------------------------------
--  Conversion from TFG.Typ
---------------------------------------------------------------------------------
instance Cnv (TFG.Typ a , r) TFA.Typ where
  cnv (tt , r) = let ?r = r in case tt of
    TFG.Int       -> pure TFA.Int
    TFG.Bol       -> pure TFA.Bol
    TFG.Flt       -> pure TFA.Flt
    TFG.Arr ta tb -> TFA.Arr <$@> ta <*@> tb
    TFG.Tpl tf ts -> TFA.Tpl <$@> tf <*@> ts
    TFG.Ary ta    -> TFA.Ary <$@> ta
    TFG.May ta    -> TFA.May <$@> ta
    TFG.Cmx       -> pure TFA.Cmx

---------------------------------------------------------------------------------
--  Conversion from TH.Typ
---------------------------------------------------------------------------------
instance Cnv (TH.Typ (TH.EnvFld '[]) , r) TFA.Typ where
  cnv (th , r) = let ?r = r in case th of
    TH.Int       -> pure TFA.Int
    TH.Bol       -> pure TFA.Bol
    TH.Flt       -> pure TFA.Flt
    TH.Arr ta tb -> TFA.Arr <$@> ta <*@> tb
    TH.Tpl tf ts -> TFA.Tpl <$@> tf <*@> ts
    TH.Ary t     -> TFA.Ary <$@> t
    TH.May t     -> TFA.May <$@> t
    TH.Cmx       -> pure TFA.Cmx
    _            -> fail ("Type Error:\n" ++ show th)

instance ts ~ ts' => Cnv (TFG.Typ ts, r) (TFG.Typ ts') where
  cnv = pure . fst