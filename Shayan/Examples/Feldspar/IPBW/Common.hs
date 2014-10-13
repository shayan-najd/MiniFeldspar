module Examples.Feldspar.IPBW.Common where

import MyPrelude

import System.IO.Unsafe

tstPPM :: [Integer]
tstPPM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.ppm"
             let "P3" : _ : "255" : c = lines f
             return (fmap (read :: String -> Integer) c))

tstPGM :: [Integer]
tstPGM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.pgm"
             let "P2" : _ : "255" : c = lines f
             return (fmap (read :: String -> Integer) c))

tstPBM :: [Integer]
tstPBM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.pbm"
             let "P1" : _ : "255" : c = lines f
             return (fmap (read :: String -> Integer) c))

loaderC :: String
loaderC = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"ImageBig.pgm\");\
\\n  AryInt  aryIn = newAryInt(size(imgIn)); \
\\n  for (Int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAryInt(aryIn , i , imgIn.data[i]);\
\\n  AryInt aryOut;\
\\n  func(aryIn , &aryOut);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 1,\
\\n                  .data  = malloc(lenAryInt(aryOut) * sizeof(Int))}; \
\\n  for(Int i = 0; i < lenAryInt(aryOut); i++)\
\\n    imgOut.data[i] = indAryInt(aryOut , i);\
\\n  writeImage (\"ImageIPBW.pbm\" , imgOut);\
\\n  return 0;\
\\n}"
