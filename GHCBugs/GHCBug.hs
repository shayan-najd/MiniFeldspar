{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction
           , FlexibleContexts #-}
class C1 t where {}  
instance C1 Integer where {}  
 
class C2 t1 t2 where {}
instance C2 Bool Integer where {}

f = 1 :: (Num a , C1 a) => a
g = 1 :: (Num a , C2 Bool a) => a