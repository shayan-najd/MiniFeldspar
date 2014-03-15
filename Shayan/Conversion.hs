module Conversion(Cnv(..),(<$@>),(<*@>)) where

import Prelude ()
import MyPrelude

import qualified Language.Haskell.TH.Syntax as TH
 
class Cnv a b where
  cnv :: a -> ErrM b
  
instance Cnv (Integer , r) Integer where
  cnv = pure . fst
  
instance Cnv (Bool , r) Bool where
  cnv = pure . fst  
  
instance Cnv (TH.Name , r) TH.Name where
  cnv = pure . fst    
  
instance Cnv (() , r) () where
  cnv = pure . fst 

instance Cnv Integer Integer where
  cnv = pure 
  
instance Cnv Bool Bool where
  cnv = pure  
  
instance Cnv TH.Name TH.Name where
  cnv = pure    
  
instance Cnv () () where  
  cnv = pure
  
infixl 4 <$@>
(<$@>) :: (?r :: r , Cnv (a , r) a') => (a' -> b) -> a -> ErrM b
el <$@> er = el <$> cnv (er , ?r)
  
infixl 4 <*@>
(<*@>) :: (?r :: r , Cnv (a , r) a') => ErrM (a' -> b) -> a -> ErrM b
el <*@> er = el <*> cnv (er , ?r)  
 