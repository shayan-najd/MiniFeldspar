{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE PolyKinds #-}
module Data.Proxy where

data T (t :: k) = T