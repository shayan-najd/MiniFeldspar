module Examples.Feldspar.Prelude.TemplateHaskellSolution1A where

import Prelude ()
import MyPrelude hiding (fst,snd)

import Examples.Feldspar.Prelude.Environment
import VanillaPrelude 

import Language.Haskell.TH.Syntax
import qualified Expression.Feldspar.ADTUntypedNamed as FAUN
import qualified Expression.Feldspar.ADTValue        as FAV

import Conversion 
import Conversion.Expression.Feldspar ()
import Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed () 

type Data t = Q (TExp t)
type Vec  t = (Integer , Integer -> t)
 
(/==) :: Data (Integer -> Integer -> Bool)
(/==) = [|| \ e1 -> \ e2 -> not (e1 == e2) ||] 

forLoop :: Data (Integer -> s -> (Integer -> s -> s) -> s)
forLoop = [|| \ l -> \ init -> \ step -> 
              snd (whl (\ t -> (fst t) < l)
                       (\ t -> (((fst t) + 1)  
                       , (step (fst t) (snd t))))
                   (0 , init)) 
          ||]

sumv :: Data (Vec Integer -> Integer)
sumv = [|| \ v -> $$forLoop (fst v) 0 (\ i -> \ s -> s + ((snd v) i)) ||]

{-
sumv = [|| \ v -> fst (whl 
                          (\ x -> ( $$((/==)) (snd x) (fst v))) 
                          (\ s -> (((snd v) (snd s)) + (fst s) , (snd s) + 1 )) 
                          (0 , 0)) 
       ||]
-}
  
mapv :: Data ((a -> b) -> Vec a -> Vec b)
mapv = [|| \ f -> \ v -> (fst v , \ x -> f ((snd v) x)) ||] 

zipWithv :: Data ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipWithv = [|| \ f -> \ v1 -> \ v2 ->  ( min (fst v1) (fst v2) 
                                       , \ i -> f ((snd v1) i) ((snd v2) i)) ||] 
    
scalarProd :: Data (Vec Integer -> Vec Integer -> Integer)
scalarProd = [|| \ vecA -> \ vecB -> $$sumv ($$zipWithv (*) vecA vecB) ||]

axpy :: Data (Integer -> Vec Integer -> Vec Integer -> Vec Integer)
axpy = [|| \ a -> \ x -> \ y -> $$zipWithv (+) ($$mapv  (\ b ->  a * b) x) y ||]

tst :: Data Integer
tst = [|| $$scalarProd (2 , \ x -> x) (2 , \ x -> x + 1) ||]
{-
test :: Bool
test = case (do tst' :: FAUN.Exp Name <- cnv (tst , etTFG , esTH) 
                cnv (tst' , emTHFAV)) of
         Rgt (FAV.ConI x) -> x == 2
         _                -> False -}
 
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