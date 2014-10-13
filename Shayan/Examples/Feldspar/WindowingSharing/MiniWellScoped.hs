{-# LANGUAGE RebindableSyntax #-}

import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.Windowing.Common

import Conversion
import Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG
import Compiler (scompile)

import Normalization
import Normalization.Feldspar.MiniWellscoped ()
import CSE

windowing :: Vec Complex -> Vec Complex
windowing s = let l = shared (lenV s)
              in  zipWith mul (append
                               (replicate (sub l (div l 4))
                                              (cmx 1.0 0.0))
                               (replicate l (cmx 0.0 0.0))) s

inp :: Vec Complex
inp = fromList (MP.fmap (\ f -> cmx (litF f) 0.0) tstPGM)
      (cmx 0.0 0.0)

out :: [MP.Float]
out  = let FGV.Exp e = MP.frmRgt (cnv ((vec2ary MP..
                                        windowing) inp, etFGV))
       in  MP.fmap MP.realPart (MP.elems e)

prop :: MP.Bool
prop = out MP.== tstPGMW

windowingAry :: Data (Ary Complex) -> Data (Ary Complex)
windowingAry = vec2ary MP.. windowing MP.. ary2vec

main :: MP.IO ()
main = let f = MP.frmRgt
                (scompile
                 (TFG.Ary TFG.Cmx)
                 esString
                 (nrm (cseF windowingAry)))
           f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile "WindowingMiniWellScoped.c" f'