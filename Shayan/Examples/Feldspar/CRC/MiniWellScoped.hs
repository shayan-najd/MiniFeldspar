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
  
crc32 :: Vec Integer -> Data Integer        
crc32 = foldl updCrc 0 
 
updCrc :: Data Integer -> Data Integer -> Data Integer
updCrc = \ cc -> \ ch -> 
          xor 
          (xor 
           (tblVec !! 
            ((xor (xor cc 0xFFFFFFFF) ch) .&. 0xff))
           ((xor cc 0xFFFFFFFF) .>>. 8)) 
          0xFFFFFFFF
 
tblVec :: Vec Integer
tblVec = fromList (MP.fmap (\ i -> litI (MP.fromIntegral i)) tblLst) 0 
  
inp :: Vec Integer
inp = fromList 
      (MP.fmap (\ i -> litI (MP.fromIntegral i)) tstInp) 0

out :: MP.Integer
out  = let FGV.Exp e =  MP.frmRgt (cnv (crc32 inp , etFGV)) 
       in e
  
prop :: MP.Bool
prop = test out

crcAry :: Data (Ary Integer) -> Data Integer
crcAry = crc32 MP.. ary2vec

main :: MP.IO ()
main = MP.getArgs MP.>>=
       (\ [as] -> let f  = MP.frmRgt 
                           (scompileWith [] 
                            TFG.Int 
                            esString 0  
                            (nrmIf (as MP./= "NoNrm") crcAry))
                      f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC    
                  in  MP.writeFile (as MP.++ "CRCMiniWellScoped.c") f') 