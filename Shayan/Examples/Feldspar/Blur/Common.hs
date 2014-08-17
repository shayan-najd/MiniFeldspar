module Examples.Feldspar.Blur.Common where

import MyPrelude

loaderC :: String
loaderC = "\nint main() \
\\n{\
\\n  Flt f;\
\\n  func (&f);\
\\n  printf(\"%f\\n\", f);\
\\n  return 0;\
\\n}"