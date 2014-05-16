{-# LANGUAGE RebindableSyntax #-}

import Prelude ()
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.CRC.Common

import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG
import Compiler (scompileWith)
  
import Normalization
import Normalization.Feldspar.MiniWellScoped ()
  
crc32 :: Vec (Data Integer) -> Data Integer        
crc32 = foldl updCrc 0 
 
updCrc :: Data Integer -> Data Integer -> Data Integer
updCrc = \ cc -> \ ch -> 
          xor 
          (xor 
           (tblVec !! 
            ((xor (xor cc 0xFFFFFFFF) ch) .&. 0xff))
           ((xor cc 0xFFFFFFFF) .>>. 8)) 
          0xFFFFFFFF
 
tblVec :: Vec (Data Integer)
tblVec = fromList (MP.fmap (\ i -> litI (MP.fromIntegral i)) tblLst) 0 
  
inp :: Vec (Data Integer)
inp = fromList 
      (MP.fmap (\ i -> litI (MP.fromIntegral i)) tstInp) 0

out :: MP.Integer
out  = let FGV.Exp e =  MP.frmRgt (cnv (crc32 inp , etFGV)) 
       in e
  
prop :: MP.Bool
prop = test out

main :: MP.IO ()
main = let f = MP.frmRgt (scompileWith [] TFG.Int esString 0  
                          (nrm (crc32 MP.. ary2vec)))
         in MP.writeFile "CRCMiniWellScoped.c" f 