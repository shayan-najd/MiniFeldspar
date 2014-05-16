module Examples.Feldspar.IP.Feldspar where

import Prelude (IO)
import Examples.Feldspar.Prelude.Feldspar 

import Feldspar.Compiler (icompile)
 
-- Conversion from grayscale to black & white 
toBW :: Vec Integer -> Vec Integer
toBW = map (\x -> condition (x < 135) 1 0) 

-- The standard red channel grayscale coefficient
redCoefficient :: Data Integer
redCoefficient   = 30

-- The standard green channel grayscale coefficient
greenCoefficient :: Data Integer
greenCoefficient = 59

-- The standard blue channel grayscale coefficient
blueCoefficient :: Data Integer
blueCoefficient  = 11

-- Conversion from RGB to grayscale
rgbToGray :: Data Integer -> Data Integer -> 
             Data Integer -> Data Integer 
rgbToGray r g b = ((r * redCoefficient  ) +                        
                   (g * greenCoefficient) +
                   (b * blueCoefficient )) / 100
                 
-- Conversion from colored to grayscale       
toGray :: Vec Integer -> Vec Integer
toGray v = vec ((length v) / 3)
           (\ i -> let j = i * 3
                        in rgbToGray 
                           (v !! j) 
                           (v !! (j + 1)) 
                           (v !! (j + 2)))
  
-- Conversion from colored to black and white
fromColoredtoBW :: Vec Integer -> Vec Integer
fromColoredtoBW v = toBW (toGray v)

toGrayC :: IO ()
toGrayC = icompile toGray