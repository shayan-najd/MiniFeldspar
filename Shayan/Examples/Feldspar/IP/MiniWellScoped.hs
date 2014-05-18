{-# LANGUAGE RebindableSyntax #-}

import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped 
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.IP.Common

import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV 
import qualified Type.Feldspar.GADT            as TFG
import Compiler (scompileWith)
 
import Normalization
import Normalization.Feldspar.MiniWellScoped ()

toBW :: Vec Integer -> Vec Integer
toBW = map (\x -> if x < 135 then 1 else 0) 

redCoefficient :: Data Integer
redCoefficient   = 30

greenCoefficient :: Data Integer
greenCoefficient = 59

blueCoefficient :: Data Integer
blueCoefficient  = 11

rgbToGray :: Data Integer -> Data Integer -> 
             Data Integer -> Data Integer 
rgbToGray r g b = ((r * redCoefficient  ) +                        
                   (g * greenCoefficient) +
                   (b * blueCoefficient )) / 100
                 
toGray :: Vec Integer -> Vec Integer
toGray v = vec ((length v) / 3)
           (\ i -> let j = i * 3
                   in rgbToGray 
                      (v !! j) 
                      (v !! (j + 1)) 
                      (v !! (j + 2)))
  
fromColoredtoBW :: Vec Integer -> Vec Integer
fromColoredtoBW v = toBW (toGray v)
 
inp :: Vec Integer
inp = fromList 
      (MP.fmap (\ i -> litI (MP.fromIntegral i)) tstPPM) 0

out :: [MP.Integer]
out  = let FGV.Exp e =  MP.frmRgt (cnv ((vec2ary MP.. 
                                         fromColoredtoBW) inp, etFGV)) 
       in  MP.elems e

prop :: MP.Bool
prop = out MP.== tstPBM

main :: MP.IO ()
main = let f = MP.frmRgt 
                  (scompileWith [] (TFG.Ary TFG.Int) esString 0 
                  (nrm (vec2ary MP.. fromColoredtoBW MP.. ary2vec)))
       in  MP.writeFile "IPMiniWellScoped.c" f    
 
