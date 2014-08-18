{-# LANGUAGE RebindableSyntax #-}

import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.IP.Common

import Conversion
import Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG
import Compiler (scompileWith)

toBW :: Vec Integer -> Vec Integer
toBW = map (\x -> if lt x 135 then 1 else 0)

redCoefficient :: Data Integer
redCoefficient   = 30

greenCoefficient :: Data Integer
greenCoefficient = 59

blueCoefficient :: Data Integer
blueCoefficient  = 11

rgbToGray :: Data Integer -> Data Integer ->
             Data Integer -> Data Integer
rgbToGray = \ r -> \ g -> \ b ->
            div
            (add
             (add (mul r redCoefficient)
                      (mul g greenCoefficient))
             (mul b blueCoefficient)) 100

toGray :: Vec Integer -> Vec Integer
toGray = \ v -> vec (div (lenV v) 3)
         (\ i -> let j = mul i 3
                 in  rgbToGray
                         (indV v j)
                         (indV v (add j 1))
                         (indV v (add j 2)))

fromColoredtoBW :: Vec Integer -> Vec Integer
fromColoredtoBW = \ v -> toBW (toGray v)

inp :: Vec Integer
inp = fromList (MP.fmap (\ i -> litI (MP.fromIntegral i)) tstPPM) 0

out :: [MP.Integer]
out  = let FGV.Exp e = MP.frmRgt (cnv ((vec2ary MP..
                                        fromColoredtoBW) inp, etFGV))
       in  MP.elems e

prop :: MP.Bool
prop = out MP.== tstPBM

fromColoredtoBWAry :: Data (Ary Integer) -> Data (Ary Integer)
fromColoredtoBWAry = vec2ary MP.. fromColoredtoBW MP.. ary2vec

main :: MP.IO ()
main = MP.getArgs MP.>>=
       (\ [as] -> let
            f = MP.frmRgt
                (scompileWith []
                 (TFG.Ary TFG.Int)
                 esString 0
                 fromColoredtoBWAry)
            f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
       in  MP.writeFile (as MP.++ "IPMiniWellScoped.c") f')