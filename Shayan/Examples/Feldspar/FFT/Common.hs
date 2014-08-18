module Examples.Feldspar.FFT.Common where

import MyPrelude

test :: [Float] -> Bool
test out = out ==
           [6.2831855,16.418755,8.885766,6.800871
           ,6.2831855,6.8008704,8.885766,16.418755]

tstInp :: [Float]
tstInp = [-4.71238898038469,
          -3.141592653589793,
          -1.5707963267948966,
          0.0,
          1.5707963267948966,
          3.141592653589793,
          4.71238898038469,
          6.283185307179586]

loaderC :: String
loaderC = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Image.pgm\");\
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
\\n  writeImage (\"ImageFFT.pgm\" , imgOut);\
\\n  return 0;\
\\n}"
