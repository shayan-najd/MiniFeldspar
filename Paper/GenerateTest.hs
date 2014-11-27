module GenerateTest where

import Prelude
import Language.Haskell.TH hiding (Type)
import Text.Regex.Posix
import Data.List hiding (foldl)

generateTest :: ExpQ
generateTest = do loc <- location
                  file <- runIO $ readFile $ loc_filename loc
                  let ns = nub $ filter (=~"^ex[0-9]+") $ map fst
                          $ concat $ map lex $ lines file
                  return (foldl (\ e n -> InfixE (Just e)
                                 (VarE '(&&)) (Just (VarE (mkName n))))
                          (ConE 'True) ns)
