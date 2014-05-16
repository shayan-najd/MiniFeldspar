module Examples.Feldspar.FFT.Common where

import Prelude ()
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
