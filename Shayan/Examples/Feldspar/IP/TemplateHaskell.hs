module Examples.Feldspar.IP.TemplateHaskell where
  
import Prelude ()
import MyPrelude hiding (snd, fst)

import Examples.Feldspar.Prelude.TemplateHaskell

-- Conversion from grayscale to black & white 
toBW :: Vec Integer -> Vec Integer
toBW = mapv [|| \ x -> if  (x < 135) then 1 else 0 ||]
        
-- The standard red channel grayscale coefficient
redCoefficient :: Data Integer
redCoefficient   = [|| 30 ||]

-- The standard green channel grayscale coefficient
greenCoefficient :: Data Integer
greenCoefficient = [|| 59 ||]

-- The standard blue channel grayscale coefficient
blueCoefficient :: Data Integer
blueCoefficient  = [|| 11 ||]
 
-- Conversion from RGB to grayscale
rgbToGray :: Data (Integer -> Integer -> Integer -> Integer)
rgbToGray = [|| \ r -> \ g -> \ b -> 
                 div ((r * $$redCoefficient  ) +                        
                      (g * $$greenCoefficient) +
                      (b * $$blueCoefficient )) 100
            ||] 
                                    
-- Conversion from colored to grayscale       
toGray :: Vec Integer -> Vec Integer
toGray (ixf , l) = ([|| \ i ->  $$rgbToGray 
                                ($$ixf (i * 3)) 
                                ($$ixf ((i * 3) + 1)) 
                                ($$ixf ((i * 3) + 2)) ||] 
                   , [|| div $$l 3 ||])                      
  
-- Conversion from colored to black and white
fromColoredtoBW :: Vec Integer -> Vec Integer
fromColoredtoBW = toBW . toGray
           
main :: IO ()          
main = do let filePPM = "Examples/Feldspar/IP/Image/Lena/Image2.ppm"
--          let filePGM = "Examples/Feldspar/IP/Image/Lena/Image2.pgm"
          let filePBM = "Examples/Feldspar/IP/Image/Lena/Image2.pbm"    
          f <- readFile filePPM
          let "P3" : s : "255" : c = lines f
--          let pgm = unlines ("P2" : s : "255" 
--                           : (fmap show  
--                              . cnvLst . toGray . cnvVec 
--                              . fmap (read :: String -> Integer)) c)            
          let pbm = unlines ("P1" : s : "255" 
                             : (fmap show  
                                . cnvLst . fromColoredtoBW . cnvVec 
                                . fmap (read :: String -> Integer)) c)  
--          writeFile filePGM  pgm      
          writeFile filePBM  pbm