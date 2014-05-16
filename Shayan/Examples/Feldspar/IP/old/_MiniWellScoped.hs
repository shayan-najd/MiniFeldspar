module Examples.Feldspar.IP.MiniWellScoped where

import Prelude ()
import MyPrelude hiding (div)

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Expression.Feldspar.MiniWellScoped

import qualified Type.Feldspar.ADT                   as TFA
import qualified Type.Feldspar.GADT                  as TFG

import Compiler

-- Conversion from grayscale to black & white 
toBW :: Vec (Data TFA.Int) -> Vec (Data TFA.Int)
toBW = mapv (\x -> Cnd (x <. (ConI 135)) (ConI 1) (ConI 0)) 

-- The standard red channel grayscale coefficient
redCoefficient :: Data TFA.Int
redCoefficient   = ConI 30

-- The standard green channel grayscale coefficient
greenCoefficient :: Data TFA.Int
greenCoefficient = ConI 59

-- The standard blue channel grayscale coefficient
blueCoefficient :: Data TFA.Int
blueCoefficient  = ConI 11

-- Conversion from RGB to grayscale
rgbToGray :: Data TFA.Int -> Data TFA.Int -> 
             Data TFA.Int -> Data TFA.Int 
rgbToGray r g b = div ((r *. redCoefficient  ) +.                        
                       (g *. greenCoefficient) +.
                       (b *. blueCoefficient )) (ConI 100)
                   
-- Conversion from colored to grayscale       
toGray :: Vec (Data TFA.Int) -> Vec (Data TFA.Int)
toGray (l , ixf) = ( div l (ConI 3) 
                   , \ i -> let j = i *. (ConI 3)
                            in rgbToGray 
                                 (ixf j) 
                                 (ixf (j +. (ConI 1))) 
                                 (ixf (j +. (ConI 2))))
  
-- Conversion from colored to black and white
fromColoredtoBW :: Vec (Data TFA.Int) -> Vec (Data TFA.Int)
fromColoredtoBW = toBW . toGray

toGray' :: Data (TFA.Ary TFA.Int) -> Data (TFA.Ary TFA.Int)
toGray' ea = uncurry Ary (toGray (Len ea , Ind ea))

toGrayC :: IO ()
toGrayC = (putStrLn . frmRgt) 
          (scompileWith [] (TFG.Ary TFG.Int) esString toGray')

{-           
main :: IO ()          
main = do let filePPM = "Examples/Feldspar/IP/Image/Lena/Image.ppm"
          let filePGM = "Examples/Feldspar/IP/Image/Lena/Image.pbm"
          f <- readFile filePPM
          let "P3" : s : "255" : c = lines f
          let v' = unlines ("P1" : s : "255" 
                           : (fmap (\ (V.Exp x) -> show x) 
                             . cnvLst . fromColoredtoBW . cnvVec 
                             . fmap (V.Exp . (read :: String -> Integer))) c)  
          writeFile filePGM  v'      
-}          