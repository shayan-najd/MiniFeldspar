{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module UExp where 

data UVal where
  I :: Int -> UVal
  F :: (UVal -> UVal) -> UVal

instance Show UVal where
  show (I n)  =  show n
  show _      =  error "A function cannot be printed!" 
instance Eq UVal where
  I m == I n  =  m == n
  _   == _    =  error "A function cannot be compared!"
  
type UEnv = [UVal]

type EmptyUEnv = UEnv

emptyUEnv :: EmptyUEnv
emptyUEnv = []

data UVar where
  UZ :: UVar
  US :: UVar -> UVar

data UExp where
  UCon :: Int -> UExp
  UAdd :: UExp -> UExp -> UExp
  UVar :: UVar -> UExp
  UAbs :: UExp -> UExp
  UApp :: UExp -> UExp -> UExp

ufetch :: UVar -> UEnv -> UVal
ufetch UZ     (a:_)  =  a
ufetch (US x) (_:e)  =  ufetch x e
ufetch _      _      =  error "Scope Error!"

ueval :: UExp -> UEnv -> UVal
ueval (UCon n)   _  =  I n
ueval (UAdd m n) e  =  ueval m e `add` ueval n e
  where I l `add` I r = I (l+r)
        _   `add` _   =  error "Type mismatch!"
ueval (UVar x)   e  =  ufetch x e
ueval (UAbs b)   e  =  F (\a -> ueval b (a:e))
ueval (UApp f a) e  =  ueval f e `app` ueval a e
  where F g `app` x  =  g x
        _   `app` _  =  error "Type mismatch!"

udouble :: UExp
udouble = UAbs (UVar UZ `UAdd` UVar UZ)

ucompose :: UExp
ucompose = UAbs (UAbs (UAbs (UVar (US (US UZ)) 
                             `UApp` (UVar (US UZ) `UApp` UVar UZ))))

ufourth :: UExp
ufourth = ucompose `UApp` udouble `UApp` udouble

ufour :: UExp
ufour = (ucompose `UApp` udouble `UApp` udouble) `UApp` (UCon 1)

ucheck :: Bool
ucheck = (ueval ufour emptyUEnv == I 4)
