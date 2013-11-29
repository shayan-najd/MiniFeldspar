{-# LANGUAGE GADTs, RankNTypes #-}

-- Simply-Typed de Bruijn in Haskell with ADTs and GADTs
-- Philip Wadler, 7--22 Nov 2013
-- Based on a blog post by Lennart Augustsson
--   http://augustss.blogspot.se/2009/06/more-llvm-recently-someone-asked-me-on.html

module DeBruijnGADT where
import Control.Monad (mzero,guard)
import Data.Maybe (fromJust)
import Prelude hiding (exp)

-- Simply-typed de Bruijn with ADTs

data Val where
  I :: Int -> Val
  F :: (Val -> Val) -> Val

instance Show Val where
  show (I i)  =  show i

instance Eq Val where
  I i == I j  =  i == j

data UTyp where
  UInt :: UTyp
  UFun :: UTyp -> UTyp -> UTyp
  deriving (Eq,Show)

data UVar where
  UZ :: UVar
  US :: UVar -> UVar

data UExp where
  UCon :: Int -> UExp
  UAdd :: UExp -> UExp -> UExp
  UVar :: UVar -> UExp
  UAbs :: UTyp -> UExp -> UExp
  UApp :: UExp -> UExp -> UExp

ufetch :: UVar -> [a] -> a
ufetch UZ (v:r)        =  v
ufetch (US x) (v:r)    =  ufetch x r

ueval :: UExp -> [Val] -> Val
ueval (UCon i) r       =  I i
ueval (UAdd m n) r     =  ueval m r `add` ueval n r
                       where I i `add` I j = I (i+j)
ueval (UVar x) r       =  ufetch x r
ueval (UAbs t n) r     =  F (\v -> ueval n (v:r))
ueval (UApp l m) r     =  ueval l r `app` ueval m r
                       where F f `app` v = f v

uinfer :: UExp -> [UTyp] -> Maybe UTyp
uinfer (UCon i) r      =  return UInt
uinfer (UAdd m n) r    =  do UInt <- uinfer m r
                             UInt <- uinfer n r
                             return UInt
uinfer (UVar x) r      =  return (ufetch x r)
uinfer (UAbs t n) r    =  do u <- uinfer n (t:r)
                             return (UFun t u)
uinfer (UApp l m) r    =  do UFun t u <- uinfer l r
                             t' <- uinfer m r
                             guard (t == t')
                             return u

udouble :: UExp
udouble = UAbs UInt (UVar UZ `UAdd` UVar UZ)

ucompose :: UTyp -> UTyp -> UTyp -> UExp
ucompose s t u = UAbs (UFun t u) (UAbs (UFun s t) (UAbs s
                   (UVar (US (US UZ)) `UApp` (UVar (US UZ) `UApp` UVar UZ))))

ufour :: UExp
ufour =  (ucompose UInt UInt UInt `UApp` udouble `UApp` udouble) `UApp` (UCon 1)

utest :: Bool
utest =  (ueval ufour [] == I 4) && (uinfer ufour [] == return UInt)

-- Simply-typed de Bruijn with GADTs

data Typ a where
  Int :: Typ Int
  Fun :: Typ a -> Typ b -> Typ (a -> b)

data Env e where
  Emp :: Env ()
  Ext :: Env e -> Typ a -> Env (e,a)

data Var e a where
  Z :: Var (e,a) a
  S :: Var e a -> Var (e,b) a

data Exp e a where
  Con :: Int -> Exp e Int
  Add :: Exp e Int -> Exp e Int -> Exp e Int
  Var :: Var e a -> Exp e a
  Abs :: Typ a -> Exp (e,a) b -> Exp e (a -> b)
  App :: Exp e (a -> b) -> Exp e a -> Exp e b

fetch :: Var e a -> e -> a
fetch Z (r,v)        =  v
fetch (S x) (r,v)    =  fetch x r

eval :: Exp e a -> e -> a
eval (Con i) r       =  i
eval (Add m n) r     =  eval m r + eval n r
eval (Var x) r       =  fetch x r
eval (Abs t n) r     =  \v -> eval n (r,v)
eval (App l m) r     =  eval l r (eval m r)

ifetch :: Var e a -> Env e -> Typ a
ifetch Z (Ext r t)      =  t
ifetch (S x) (Ext r t)  =  ifetch x r

infer :: Exp e a -> Env e -> Maybe (Typ a)
infer (Con i) r     =  return Int
infer (Add m n) r   =  do Int <- infer m r
                          Int <- infer n r
                          return Int
infer (Var x) r      =  return (ifetch x r)
infer (Abs t n) r    =  do u <- infer n (Ext r t)
                           return (Fun t u)
infer (App l m) r    =  do Fun t u <- infer l r
                           t' <- infer m r
                           Eq <- eqTyp t t'
                           return u

double :: Exp () (Int -> Int)
double = Abs Int (Var Z `Add` Var Z)

compose :: Typ a -> Typ b -> Typ c -> Exp () ((b -> c) -> (a -> b) -> (a -> c))
compose s t u = Abs (Fun t u) (Abs (Fun s t) (Abs s
                  (Var (S (S Z)) `App` (Var (S Z) `App` Var Z))))

four :: Exp () Int
four = (compose Int Int Int `App` double `App` double) `App` (Con 1)

test :: Bool
test = (eval four () == 4) && fromJust (do Int <- infer four Emp ; return True)

-- Type and environment equality

data Equal a b where
  Eq :: Equal a a

eqTyp :: Typ a -> Typ a' -> Maybe (Equal a a')
eqTyp Int Int                =  return Eq
eqTyp (Fun t u) (Fun t' u')  =  do Eq <- eqTyp t t'
                                   Eq <- eqTyp u u'
                                   return Eq                    
eqTyp _ _                    =  mzero

eqEnv :: Env e -> Env e' -> Maybe (Equal e e')
eqEnv Emp Emp                =  return Eq
eqEnv (Ext e a) (Ext e' a')  =  do Eq <- eqEnv e e'
                                   Eq <- eqTyp a a'
                                   return Eq
eqEnv _ _                    =  mzero     

-- Wrappers

data WTyp where
  WTyp :: Typ a -> WTyp

data WEnv where
  WEnv :: Env a -> WEnv

data WVar where
  WVar :: Var e a -> Env e -> Typ a -> WVar

data WExp where
  WExp :: Exp e a -> Env e -> Typ a -> WExp

-- Translate ADTs to GADTs

typ :: UTyp -> Maybe WTyp
typ UInt          =  return (WTyp Int)
typ (UFun t u)    =  do WTyp t' <- typ t
                        WTyp u' <- typ u
                        return (WTyp (Fun t' u'))


env :: [UTyp] -> Maybe WEnv
env []            =  return (WEnv Emp)
env (t:r)         =  do WTyp t' <- typ t
                        WEnv r' <- env r
                        return (WEnv (Ext r' t'))

var :: UVar -> [UTyp] -> Maybe WVar
var UZ (t:r)      =  do WEnv r' <- env r
                        WTyp t' <- typ t
                        return (WVar Z (Ext r' t') t')
var (US x) (u:r)  =  do WVar x' r' t' <- var x r
                        WTyp u' <- typ u
                        return (WVar (S x') (Ext r' u') t')

exp :: UExp -> [UTyp] -> Maybe WExp
exp (UCon i) r    =  do WEnv r' <- env r
                        return (WExp (Con i) r' Int)
exp (UAdd m n) r  =  do WExp m' r'  Int <- exp m r
                        WExp n' r'' Int <- exp n r
                        Eq <- eqEnv r' r''
                        return (WExp (Add m' n') r' Int)
exp (UVar x) r    =  do WVar x' r' t' <- var x r
                        return (WExp (Var x') r' t')
exp (UAbs t n) r  =  do WTyp t' <- typ t
                        WExp n' (Ext r' t'') u' <- exp n (t:r)
                        Eq <- eqTyp t' t''
                        return (WExp (Abs t' n') r' (Fun t' u'))
exp (UApp l m) r  =  do WExp l' r' (Fun t' u') <- exp l r
                        WExp m' r'' t'' <- exp m r
                        Eq <- eqEnv r' r''
                        Eq <- eqTyp t' t''
                        return (WExp (App l' m') r' u')

evaluate :: UExp -> Int
evaluate m  =  fromJust (do WExp m' Emp Int <- exp m []
                            return (eval m' ()))

tst :: Bool
tst = (evaluate ufour == 4)

main :: Bool
main = utest && test && tst

-- A possible way to encode existentials -- requires RankNTypes.

typ' :: UTyp -> (forall a. Typ a -> b) -> b
typ' UInt k          =  k Int
typ' (UFun t u) k    =  typ' t (\ t' ->
                        typ' u (\ u' ->
                        k (Fun t' u')))

{-
    Couldn't match type `a0 -> b0' with `Int'
    Expected type: Typ a
      Actual type: Typ Int
    In the first argument of `k', namely `Int'
    In the expression: k Int
    Failed, modules loaded: none.
-}
