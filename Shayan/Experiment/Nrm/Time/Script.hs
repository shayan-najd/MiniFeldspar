import Data.List
import Data.List.Utils
import System.Directory
import Control.DeepSeq

(</>) :: String -> String -> String
d </> f = d ++ "/" ++ f

main :: IO()
main = do let d = "./Experiment/Not-Nrm/Time" 
          fs <- fmap (delete "Script.hs" . delete "." . delete "..") $ 
                getDirectoryContents d
          avgs <- mapM (\ f -> 
                         do fc <- readFile (d </> f)
                            -- deepseq fc $ writeFile (d </> f) (g fc)
                            -- renameFile (d </> f) (d </> (f ++ ".csv"))
                            return (f 
                                   , sum (map ((read :: String -> Float) 
                                               . head . wordsWhen (== ',')) 
                                          (lines fc)) / 10)
                       ) fs
          putStrLn (show avgs)        

g :: String -> String
g = replace "0m" "" . 
    replace "s" "" . 
    replace "\nreal\t" "" . 
    replace "\nsys\t" "," . 
    replace "\nuser\t" "," . 
    replace "\n\nreal\t" "\n" 

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'