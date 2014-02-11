{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Singleton where
  
class Sin s t where
  sin :: s t
  
  