{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Singleton where
  
class HasSin tf t where
  sin :: tf t
 