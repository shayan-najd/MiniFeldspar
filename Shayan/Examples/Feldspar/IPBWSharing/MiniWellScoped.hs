{-# LANGUAGE RebindableSyntax #-}

import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.IPBW.Common

import Conversion
import Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG
import Compiler (scompileWith)

import Normalization
import Normalization.Feldspar.MiniWellscoped ()
import CSE

toBW :: Vec Integer -> Vec Integer
toBW = map (\x -> if lt x 135 then 1 else 0)

inp :: Vec Integer
inp = fromList (MP.fmap (\ i -> litI (MP.fromIntegral i)) tstPPM) 0

out :: [MP.Integer]
out  = let FGV.Exp e = MP.frmRgt (cnv ((vec2ary MP.. toBW) inp, etFGV))
       in  MP.elems e

prop :: MP.Bool
prop = out MP.== tstPBM

toBWAry :: Data (Ary Integer) -> Data (Ary Integer)
toBWAry = vec2ary MP.. toBW MP.. ary2vec

main :: MP.IO ()
main = let f = MP.frmRgt
               (scompileWith []
                (TFG.Ary TFG.Int)
                esString 0
                (nrm (cseF toBWAry)))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile "IPBWMiniWellScoped.c" f'