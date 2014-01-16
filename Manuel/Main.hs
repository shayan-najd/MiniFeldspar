{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable

import qualified DeBruijn as F
import qualified HOAS     as H
import Convert

i = H.Lam $ \x -> x

zero  = H.Lam $ \f -> H.Lam $ \x -> x
one   = H.Lam $ \f -> H.Lam $ \x -> f `H.App` x
two   = H.Lam $ \f -> H.Lam $ \x -> f `H.App` (f `H.App` x)
three = H.Lam $ \f -> H.Lam $ \x -> 
          f `H.App` (f `H.App` (f `H.App` x))

plus = H.Lam $ \m -> 
       H.Lam $ \n -> 
       H.Lam $ \f -> 
       H.Lam $ \x -> m `H.App` f `H.App` (n `H.App` f `H.App` x)

plusTwoThree = plus `H.App` two `H.App` three


pair = H.Lam $ \x -> 
       H.Lam $ \y -> 
       H.Lam $ \z -> z `H.App` x `H.App` y
pairfst = H.Lam $ \p -> p `H.App` (H.Lam $ \x -> H.Lam $ \_ -> x)
pairsnd = H.Lam $ \p -> p `H.App` (H.Lam $ \_ -> H.Lam $ \y -> y)

pairfstPair = pairfst `H.App` 
                (pair `H.App` (H.Con 'a') `H.App` (H.Con 'b'))

data Nat = Zro 
         | Suc Nat 
         deriving (Show, Typeable)

{-
main 
  = do
      printLine "Identity           :"     (convert i)
      printLine "zero               :"     (convert zero)
      printLine "one                :"     (convert one)
      printLine "two                :"     (convert two)
      printLine "three              :"     (convert three)
      printLine "plus               :"     (convert plus)
      printLine "plus two three     :\n  " (convert plusTwoThree)
      printLine "EVAL plus two three:" 
        ((F.evl (convert plusTwoThree)) F.Emp S Z)
      printLine "pairfst (pair 'a' 'b'):\n  " (convert pairfstPair)
      printLine "EVAL pairfst (pair 'a' 'b'):"   
        (F.evl (convert pairfstPair) F.Emp)
  where
    printLine desc e = putStrLn $ desc ++ " " ++ show e
-}