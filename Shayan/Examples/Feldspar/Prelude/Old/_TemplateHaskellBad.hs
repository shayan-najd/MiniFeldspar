{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Examples.Feldspar.Prelude.TemplateHaskellBad where

import Prelude ()
import MyPrelude hiding (fst,snd)
import qualified Expression.Feldspar.ADTValue        as FAV
import VanillaPrelude 

import Language.Haskell.TH.Syntax
import qualified Expression.Feldspar.ADTUntypedNamed as FAUN

import Conversion 
import Conversion.Expression.Feldspar ()
import Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed ()

import Examples.Feldspar.Prelude.Environment 

type Data t = Q (TExp t)
type Vec  t = (Data (Integer -> t) , Data Integer)
 
(/==) :: Data (Integer -> Integer -> Bool)
(/==) = [|| \ e1 -> \ e2 -> not (e1 == e2) ||] 

sumv :: Vec Integer -> Data Integer
sumv (ixf , l) = [|| fst (whl 
                          (\ x -> ( $$((/==)) (snd x) $$l)) 
                          (\ s -> (($$ixf (snd s)) + (fst s) , (snd s) + 1 )) 
                          (0 , 0)) 
                  ||]
  
mapv :: Data (a -> b) -> Vec a -> Vec b
mapv f (ixf,l) = ([|| \ x -> $$f ($$ixf x) ||] , l)

zipWithv :: Data (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf , [|| min $$l1 $$l2 ||])
  where ixf = [|| \ i -> $$f ($$ixf1 i) ($$ixf2 i) ||]
 

scalarProd :: Vec Integer -> Vec Integer -> Data Integer
scalarProd vecA vecB = sumv (zipWithv [|| (*) ||] vecA vecB)

axpy :: Data Integer -> Vec Integer -> Vec Integer -> Vec Integer
axpy a x y = zipWithv [|| (+) ||] (mapv [|| \ b ->  $$a * b ||] x) y

tst :: Data Integer
tst = scalarProd ([||\ x -> x ||] , [|| 2 ||]) ([||\ x -> x + 1 ||] , [|| 2 ||])

test :: Bool
test = case (do tst' :: FAUN.Exp Name <- cnv (tst , etTFG , esTH) 
                cnv (tst' , emTHFAV)) of
         Rgt (FAV.ConI x) -> x == 2
         _                -> False
         
--forLoop :: Data Integer -> Data s -> 
--           (Data TFA.Int -> Data s -> Data s ) -> Data s
forLoop :: Data Integer -> Data s -> Data (Integer -> s -> s) -> Data s
forLoop l init step = [|| snd (whl (\ t -> (fst t) < $$l)
                                   (\ t -> (((fst t) + 1)  
                                           ,($$step (fst t) (snd t))))
                                   (0 , $$init)) ||]
                  
{-

cnvVec :: [Integer] -> Vec Integer
cnvVec vss = ( g vss 
            , [|| l ||] )
  where
    l  = (toInteger . length) vss
    g  = f 0 
    f _ []       = [|| \ x -> 0 ||]
    f i (v : vs) = [|| \ x -> if (x == i) 
                              then v
                              else $$(f (i+1) vs) x 
                   ||]

cnvLst :: Vec Integer -> [Integer] 
cnvLst (ixf , l) = frmRgt (
                   do ixf' :: FAUN.Exp Name <- cnv (ixf  , envTyp , envNam) 
                      FAV.Abs f             <- cnv (ixf' , envVal)
                      l'   :: FAUN.Exp Name <- cnv (l    , envTyp , envNam)
                      FAV.ConI i            <- cnv (l'   , envVal)
                      return (fmap ((\ (FAV.ConI k) -> k) . f . FAV.ConI) 
                              [0..(i-1)]))

-}