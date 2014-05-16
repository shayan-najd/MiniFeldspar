module Examples.Feldspar.Prelude.TemplateHaskellSolution2A where

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
type Vec  t = (Data Integer , Data Integer -> t)
 
(/==) :: Data (Integer -> Integer -> Bool)
(/==) = [|| \ e1 -> \ e2 -> not (e1 == e2) ||] 

forLoop :: Data Integer -> Data s -> 
           (Data Integer -> Data s -> Data s) -> Data s
forLoop l init step = [|| snd (whl (\ t -> (fst t) < $$l)
                                   (\ t -> (((fst t) + 1)  
                                   , $$(step [|| fst t ||] [|| snd t ||])))
                               (0 , $$init)) 
                      ||]

sumv :: Vec (Data Integer) -> Data Integer
sumv (l , ixf) = forLoop l [||0||] (\ i s ->  [||$$s + $$(ixf i)||])

{-
sumv (l , ixf) = [|| fst (whl 
                          (\ x -> ( $$((/==)) (snd x) $$l)) 
                          (\ s -> ($$(ixf [||snd s ||]) + (fst s) 
                                  , (snd s) + 1 )) 
                          (0 , 0)) 
                  ||]
-}
  
mapv :: (a -> b) -> Vec a -> Vec b
mapv f (l , ixf) = (l , f . ixf)

zipWithv :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWithv f (l1 , ixf1) (l2 , ixf2) = 
  ([|| min $$l1 $$l2 ||] , \ i -> f (ixf1 i) (ixf2 i))
 
scalarProd :: Vec (Data Integer) -> Vec (Data Integer) -> Data Integer
scalarProd vecA vecB = sumv (zipWithv (\ v1 v2 -> [||$$v1 * $$v2||]) vecA vecB)


axpy :: Data Integer -> Vec (Data Integer) -> Vec (Data Integer) 
        -> Vec (Data Integer)
axpy a x y = zipWithv (\ v1 v2 -> [||$$v1 + $$v2||]) 
             (mapv (\ b -> [|| $$a * $$b ||]) x) y


tst :: Data Integer
tst = scalarProd ([|| 2 ||] , \ x -> x) ([|| 2 ||] , \ x -> [|| $$x + 1 ||])

test :: Bool
test = case (do tst' :: FAUN.Exp Name <- cnv (tst , etTFG , esTH) 
                cnv (tst' , emTHFAV)) of
         Rgt (FAV.ConI x) -> x == 2
         _                -> False
         
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