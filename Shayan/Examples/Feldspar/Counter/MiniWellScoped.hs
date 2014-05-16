{-# LANGUAGE RebindableSyntax #-}
module Examples.Feldspar.Counter.MiniWellScoped where

import Prelude ()
import qualified MyPrelude as MP
import Examples.Feldspar.Prelude.MiniWellScoped hiding (forLoop,forLoopVec)
import Examples.Feldspar.Prelude.Environment
import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()
import qualified Data.Complex                  as CMPX
import qualified Expression.Feldspar.GADTValue as FGV
import Type.Feldspar.GADT                      as TFG
import Singleton
import Compiler (scompile)

prop :: MP.Bool
prop = out MP.== 
       lst

lst :: [MP.Float]
lst = [4.71238898038469,
       3.141592653589793,
       1.5707963267948966,
       0.0,
       1.5707963267948966,
       3.141592653589793,
       4.71238898038469,
       6.283185307179586]

inp :: Vec (Data Float)
inp = fromList (MP.fmap litF lst) 0.0

res :: Vec (Data Float)
res = id inp

out :: [MP.Float]
out = let FGV.Exp e = MP.frmRgt (cnv (vec2ary res , etFGV)) 
                      :: FGV.Exp (Ary Float) 
      in  MP.elems e
         
fft' :: Data (Ary Float) -> Data (Ary Float)          
fft' = \ a -> vec2ary (id (ary2vec a))

fftC :: MP.IO ()
fftC = let f = MP.frmRgt (scompile (TFG.Ary TFG.Cmx) esString fft')
       in MP.writeFile "/home/tester/Mini.c" f    

         
forLoop :: HasSin TFG.Typ s => 
           Data Integer -> Data s -> (Data Integer -> Data s -> Data s) -> Data s
forLoop = \ l -> \ init -> \step -> 
          snd (whl (\ t -> (<) (fst t) l)
                   (\ t -> tpl 
                           ((+) (fst t) 1) 
                           (step (fst t) (snd t)))
               (tpl 0 init))
 
id :: Vec (Data Float) -> Vec (Data Float)
id = \ vv -> ary2vec (forLoop 1 (vec2ary vv) (\ j -> \ v -> v))
          