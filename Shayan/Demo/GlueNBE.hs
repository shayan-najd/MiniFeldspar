-- Inspired by Dybjer and Flinisky's 
module Demo.GlueNBE where

import Prelude
import Data.Maybe

data Syn = S | K | App Syn Syn | Zero | Succ | Rec

primRec :: a -> (Int -> a -> a) -> Int -> a
primRec z _ 0 = z
primRec z s n = s (n - 1) (primRec z s (n - 1))
 
data Sem  = Fun Syn (Sem -> Maybe Sem) | Nat Int

reifyNat :: Int -> Syn
reifyNat 0 = Zero
reifyNat n = App Succ (reifyNat (n-1))

reify :: Sem -> Syn
reify (Fun syn _) = syn
reify (Nat n)     = reifyNat n

appSem :: Sem -> Sem -> Maybe Sem
appSem (Fun _ f) arg = f arg
appSem (Nat _)   _   = Nothing     

succSem :: Sem -> Maybe Sem
succSem (Fun _ _) = Nothing
succSem (Nat n)   = Just (Nat (n + 1)) 

recSem :: a -> (Int -> a -> a) -> Sem -> Maybe a
recSem _ _ (Fun _ _) = Nothing   
recSem z s (Nat n)   = Just (primRec z s n)

eval :: Syn -> Maybe Sem
eval S           = Just (Fun S (\ f -> 
                     Just (Fun (App S (reify f)) (\ g -> 
                       Just (Fun (App (App S (reify f)) (reify g)) (\ x -> 
                         do f' <- appSem f x
                            g' <- appSem g x
                            appSem f' g'))))))
eval K           = Just (Fun K (\ x -> 
                     Just (Fun (App K (reify x)) (\ _y -> Just x))))
eval (App e0 e1) = do e0' <- eval e0 
                      e1' <- eval e1
                      appSem e0' e1'
eval Zero        = Just (Nat 0) 
eval Succ        = Just (Fun Succ succSem)
eval Rec         = Just (Fun Rec (\ z -> 
                     Just (Fun (App Rec (reify z)) (\ s -> 
                       Just (Fun (App (App Rec (reify z)) (reify s)) 
                         (recSem z (\ n c -> fromJust (do asn <- appSem s (Nat n)
                                                          appSem asn c)
                                   )))))))


