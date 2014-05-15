{-# LANGUAGE RebindableSyntax #-}
 
import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped 
  (Vec,Ary,Data,Complex,Integer,forLoop,ind,len,ary,vec,length,(!!)) 
import Examples.Feldspar.Prelude.Environment

import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import qualified Type.Feldspar.GADT            as TFG
import Compiler(scompile)

import Normalization
import Normalization.Feldspar.MiniWellScoped ()

vec2ary :: Vec (Data Complex) -> Data (Ary Complex)
vec2ary v = ary (length v) (v !!) 

ary2vec :: Data (Ary Complex) -> Vec (Data Complex)
ary2vec v = vec (len v) (\i -> ind v  i)

forLoopVec :: Data Integer -> Vec (Data Complex) -> 
           (Data Integer -> Vec (Data Complex) -> Vec (Data Complex)) -> 
           Vec (Data Complex)
forLoopVec l init step =  let init'     = vec2ary init
                              step' i a = vec2ary (step i (ary2vec a))
                          in  ary2vec (forLoop l init' step')    

fft :: Vec (Data Complex) -> Vec (Data Complex)
fft = \ v -> forLoopVec (length v) v
             (\ _i -> 
               \ vv -> vec (length v) (\ i -> vv !! i))
   
main :: MP.IO ()
main = let f = MP.frmRgt (scompile (TFG.Ary TFG.Cmx) esString 
                          (nrm (vec2ary MP.. fft MP.. ary2vec)))
       in MP.putStrLn f