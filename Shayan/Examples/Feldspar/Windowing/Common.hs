module Examples.Feldspar.Windowing.Common where

import qualified Prelude as P
import MyPrelude

import System.IO.Unsafe

tstPGM :: [Float]
tstPGM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.pgm"
             let "P2" : _ : "255" : c = lines f
             return (fmap (fromInteger . (read :: String -> P.Integer)) c))

tstPGMW :: [Float]
tstPGMW = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16W.pgm"
             let "P1" : _ : "255" : c = lines f
             return (fmap (fromInteger . (read :: String -> P.Integer)) c))

loaderC :: String
loaderC = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"ImageBig.pgm\");\
\\n  AryCmx  aryIn = newAryCmx(size(imgIn)); \
\\n  for (Int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAryCmx(aryIn , i , cmx(i2f(imgIn.data[i]),0.0));\
\\n  AryCmx aryOut;\
\\n  func(aryIn , &aryOut);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 2,\
\\n                  .data  = malloc(lenAryCmx(aryOut) * sizeof(Int))}; \
\\n  for(Int i = 0; i < lenAryCmx(aryOut); i++)\
\\n    imgOut.data[i] = (Int)(floorf(cabsf(indAryCmx(aryOut , i))));\
\\n  writeImage (\"ImageWindowing.pgm\" , imgOut);\
\\n  return 0;\
\\n}"