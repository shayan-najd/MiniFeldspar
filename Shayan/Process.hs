import Prelude
import System.Directory
import System.Environment

(</>) :: String -> String -> String
d </> f = d ++ "/" ++ f

main :: IO()
main = do [d] <- getArgs
          fs <- fmap (filter ((== "time") . take 4))
                $ getDirectoryContents d
          avgs <- mapM (\ f -> do
                          fc <- readFile (d </> f)
                          let ls = lines fc
                          return (drop 4 f
                                 , {-sum-} minimum
                                               (map (read :: String -> Float) ls)
                                          {- / fromIntegral (length ls)-})
                       ) fs
          writeFile (d </> "result") (show avgs)