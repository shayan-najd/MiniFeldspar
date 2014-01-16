{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable

import qualified DeBruijn
import qualified HOAS
import Convert

i = HOAS.Lam $ \x -> x

zero  = HOAS.Lam $ \f -> HOAS.Lam $ \x -> x
one   = HOAS.Lam $ \f -> HOAS.Lam $ \x -> f `HOAS.App` x
two   = HOAS.Lam $ \f -> HOAS.Lam $ \x -> f `HOAS.App` (f `HOAS.App` x)
three = HOAS.Lam $ \f -> HOAS.Lam $ \x -> 
          f `HOAS.App` (f `HOAS.App` (f `HOAS.App` x))

plus = HOAS.Lam $ \m -> 
       HOAS.Lam $ \n -> 
       HOAS.Lam $ \f -> 
       HOAS.Lam $ \x -> m `HOAS.App` f `HOAS.App` (n `HOAS.App` f `HOAS.App` x)

plusTwoThree = plus `HOAS.App` two `HOAS.App` three

data Nat = Z | S Nat deriving (Show, Typeable)

pair = HOAS.Lam $ \x -> 
       HOAS.Lam $ \y -> 
       HOAS.Lam $ \z -> z `HOAS.App` x `HOAS.App` y
pairfst = HOAS.Lam $ \p -> p `HOAS.App` (HOAS.Lam $ \x -> HOAS.Lam $ \y -> x)
pairsnd = HOAS.Lam $ \p -> p `HOAS.App` (HOAS.Lam $ \x -> HOAS.Lam $ \y -> y)

pairfstPair = pairfst `HOAS.App` 
                (pair `HOAS.App` (HOAS.Con 'a') `HOAS.App` (HOAS.Con 'b'))

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
        ((DeBruijn.intp (convert plusTwoThree)) DeBruijn.Empty S Z)
      printLine "pairfst (pair 'a' 'b'):\n  " (convert pairfstPair)
      printLine "EVAL pairfst (pair 'a' 'b'):"   
        (DeBruijn.intp (convert pairfstPair) DeBruijn.Empty)
  where
    printLine desc e = putStrLn $ desc ++ " " ++ show e
