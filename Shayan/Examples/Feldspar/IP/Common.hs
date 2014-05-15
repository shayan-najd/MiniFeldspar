module Examples.Feldspar.IP.Common where

import Prelude ()
import MyPrelude

import System.IO.Unsafe

tstPPM :: [Integer]
tstPPM = unsafePerformIO 
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image.ppm"
             let "P3" : _ : "255" : c = lines f
             return (fmap (read :: String -> Integer) c))

tstPGM :: [Integer]
tstPGM = unsafePerformIO 
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image.pgm"
             let "P2" : _ : "255" : c = lines f
             return (fmap (read :: String -> Integer) c))
 
tstPBM :: [Integer]
tstPBM = unsafePerformIO 
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image.pbm"
             let "P1" : _ : "255" : c = lines f
             return (fmap (read :: String -> Integer) c))