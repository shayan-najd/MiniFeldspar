module Conversion(Cnv(..),(<$@>),(<*@>),cnvImp) where

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
   
instance Cnv (a , r) b => Cnv (Array Integer a , r) (Array Integer b) where
  cnv (e , r) = let ?r = r in mapM cnvImp e

cnvImp :: (Cnv (a , r) b , ?r :: r) => a -> ErrM b
cnvImp x = cnv (x  , ?r)

infixl 4 <$@>
(<$@>) :: (?r :: r , Cnv (a , r) a') => (a' -> b) -> a -> ErrM b
el <$@> er = el <$> cnv (er , ?r)
  
infixl 4 <*@>
(<*@>) :: (?r :: r , Cnv (a , r) a') => ErrM (a' -> b) -> a -> ErrM b
el <*@> er = el <*> cnv (er , ?r)  
 