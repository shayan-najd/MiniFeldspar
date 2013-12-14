{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,DeriveFunctor #-}
module ExplicitADT where

import ErrorMonad
import ADT (Var(..),Typ(..),(===),Val(..),get,app,add)

-- ADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- (a la Curry) expressions with Integer constants and a built-in addition 
-- operator
data Exp a =
    Con a Int
  | Var a Var
  | Abs a (Exp a) 
  | App a (Exp a) (Exp a)
  | Add a (Exp a) (Exp a) 
  deriving (Eq , Functor)
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    
 
-- Evaluation of expressions under specific environment of values 
evl :: Exp a -> [Val] -> ErrM Val
evl (Con _ i)     _ = return (Num i)
evl (Var _ x)     r = get x r
evl (Abs _ eb)    r = return (Fun (\ va -> case evl eb (va : r) of 
                                      Rgt vb -> vb
                                      Lft  s  -> error s))
evl (App _ ef ea) r = do vf <- evl ef r
                         va <- evl ea r
                         vf `app` va 
evl (Add _ el er) r = do vl <- evl el r 
                         vr <- evl er r      
                         vl `add` vr
                         
-- A type class providing operations required for type unification. The first 
-- argument (m) provides the monad required for unification of types (t).       
class Monad m => Chk m t where
  eql      :: t -> t -> m ()
  eqlInt   :: t -> m ()
  eqlArr   :: t -> m (t , t)
  
instance Chk ErrM Typ where
  eql                = (===) 
  eqlInt             = eql Int
  eqlArr (x `Arr` y) = return (x , y)
  eqlArr _           = fail "Type Error!"

-- Typechecking and returning the type, if successful
chk :: Chk m a => Exp a -> [a] -> m a 
chk (Con t _)     _ = do eqlInt t 
                         return t
chk (Var t x)     r = do t' <- get x r
                         eql t t'
                         return t
chk (Abs t eb)    r = do (ta , tb) <- eqlArr t    
                         tb'       <- chk eb (ta : r)
                         eql tb tb'
                         return t
chk (App t ef ea) r = do tf        <- chk ef r
                         ta'       <- chk ea r
                         (ta , tb) <- eqlArr tf
                         eql ta ta' 
                         eql t  tb
                         return t
chk (Add t el er) r = do tl <- chk el r
                         tr <- chk er r
                         eqlInt tl 
                         eqlInt tr 
                         eqlInt t 
                         return t
  
-- An example expression doubling the input number
dbl :: Exp Typ
dbl = Abs (Int `Arr` Int) (Add Int (Var Int Zro) (Var Int Zro))

-- An example expression composing two types
compose :: Typ -> Typ -> Typ -> Exp Typ
compose ta tb tc = Abs (Arr (Arr tb tc) (Arr (Arr ta tb) (Arr ta tc))) 
                (Abs (Arr (Arr ta tb) (Arr ta tc))
                 (Abs (Arr ta tc)
                  (App tc (Var (Arr tb tc) (Suc (Suc Zro))) 
                   (App tb (Var (Arr ta tb) (Suc Zro)) (Var ta Zro)))))

-- An example expression representing the Integer 4
four :: Exp Typ
four = App Int 
       (App (Arr Int Int) 
        (App (Arr (Arr Int Int) (Arr Int Int)) 
         (compose Int Int Int)  dbl) dbl) (Con Int 1)
 
-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (Num 4) -> True
          _             -> False) 
       && ((chk four [] :: ErrM Typ) == Rgt Int)
 
instance Show a => Show (Exp a) where 
  show (Con t i)                 = "(" ++ show i ++ " : " ++ show t ++ ")"
  show (Var t v)                 = "(" ++ show v ++ " : " ++ show t ++ ")"
  show (Abs t eb)                = "(\\ " ++ show eb ++ " : " ++ show t ++ ")"
  show (App t ef@(App _ _ _) ea) = "((" ++ show ef ++ ") " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (App t ef@(Add _ _ _) ea) = "((" ++ show ef ++ ") " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (App t ef ea)             = "(" ++ show ef ++ " " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (Add t el@(App _ _ _) er) = "((" ++ show el ++ ") + " ++ show er 
                                   ++ " : " ++ show t ++ ")"
  show (Add t el@(Add _ _ _) er) = "((" ++ show el ++ ") + " ++ show er 
                                   ++ " : " ++ show t ++ ")"
  show (Add t el er)             = "(" ++ show el ++ " + " ++ show er 
                                   ++ " : " ++ show t ++ ")"