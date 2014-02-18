{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds #-}
module Singleton where
  
class HasSin tf t where
  sin :: tf t
 
type family Trm (t :: k) :: *
     
type family RevTrm (t :: *) :: k     