module Examples.Feldspar.IP.TemplateHaskellSolution2B where
  
import Prelude ()
import MyPrelude hiding (fst , snd)

import Examples.Feldspar.Prelude.TemplateHaskellSolution2B
import Examples.Feldspar.Prelude.Environment
import VanillaPrelude

import qualified Language.Haskell.TH.Syntax          as TH
import qualified Expression.Feldspar.MiniWellScoped  as FMWS

import qualified Type.Feldspar.ADT                   as TFA
import qualified Type.Feldspar.GADT                  as TFG

import qualified Environment.Scoped                  as ES
import qualified Environment.Typed                   as ET

import Conversion 
import Conversion.Expression.Feldspar ()

import Compiler (scompileWith)

import qualified Nat.ADT                             as NA
import Singleton (Len)

-- Conversion from grayscale to black & white 
toBW :: Vec Integer -> Vec Integer
toBW = mapv (\ x -> [|| if ($$x < 135) then 1 else 0 ||])
        
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
rgbToGray :: Data Integer -> Data Integer -> Data Integer -> Data Integer
rgbToGray r g b = [|| div (($$r * $$redCoefficient  ) +                        
                           ($$g * $$greenCoefficient) +
                           ($$b * $$blueCoefficient )) 100 ||] 
                                 
-- Conversion from colored to grayscale       
toGray :: Vec Integer -> Vec Integer
toGray v = [|| ary (div (len $$v) 3)
                   ( \ i -> $$(rgbToGray [|| ind $$v (i * 3)       ||] 
                                         [|| ind $$v ((i * 3) + 1) ||]
                                         [|| ind $$v ((i * 3) + 2) ||])) 
           ||]
            
-- Conversion from colored to black and white
fromColoredtoBW :: Vec Integer -> Vec Integer
fromColoredtoBW = toBW . toGray
           
   
dummyVec :: Vec Integer
dummyVec = dummyVec

es :: ES.Env (NA.Suc (Len Prelude)) TH.Name
es = 'dummyVec <+> esTH      

et :: ET.Env TFG.Typ (TFA.Ary TFA.Int ': Prelude)
et = TFG.Ary TFG.Int <:> etTFG
 
toGrayFMWS :: FMWS.Exp (TFA.Ary TFA.Int ': Prelude) (TFA.Ary TFA.Int)
toGrayFMWS = frmRgt (cnv (toGray (toData 'dummyVec) , et , es)) 

toGrayC :: IO ()
toGrayC = (putStrLn . frmRgt) 
          (scompileWith [("_xa" , TFA.Ary TFA.Int)]  
           (TFG.Ary TFG.Int) ("_xa" <+> esString) toGrayFMWS)

{-
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
-}