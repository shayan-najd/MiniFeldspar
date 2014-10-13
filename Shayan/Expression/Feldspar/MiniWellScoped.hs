module Expression.Feldspar.MiniWellScoped
    (Exp(..),sucAll,prdAll,mapVar,isFresh,absTmp,eql) where

import MyPrelude hiding (foldl)

import qualified Type.Feldspar.ADT      as TFA
import qualified Type.Feldspar.GADT     as TFG

import Variable.Typed

import Environment.Typed as ET
import Data.IORef
import System.IO.Unsafe

import Singleton

data Exp :: [TFA.Typ] -> TFA.Typ -> * where
  ConI  :: Integer  -> Exp r TFA.Int
  ConB  :: Bool     -> Exp r TFA.Bol
  ConF  :: Float    -> Exp r TFA.Flt
  AppV  :: HasSin TFG.Typ t =>
           Var r t  -> Env (Exp r) (TFG.Arg t) -> Exp r (TFG.Out t)
  Cnd   :: Exp r TFA.Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r t -> Exp r TFA.Bol) -> (Exp r t -> Exp r t) ->
           Exp r t  -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (TFA.Tpl tf ts)
  Fst   :: HasSin TFG.Typ ts =>
           Exp r (TFA.Tpl tf ts)-> Exp r tf
  Snd   :: HasSin TFG.Typ tf =>
           Exp r (TFA.Tpl tf ts)-> Exp r ts
  Ary   :: Exp r TFA.Int -> (Exp r TFA.Int -> Exp r t) -> Exp r (TFA.Ary t)
  Len   :: HasSin TFG.Typ ta =>
           Exp r (TFA.Ary ta) -> Exp r TFA.Int
  Ind   :: Exp r (TFA.Ary ta) -> Exp r TFA.Int -> Exp r ta
  Let   :: HasSin TFG.Typ tl =>
           Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Cmx   :: Exp r TFA.Flt -> Exp r TFA.Flt -> Exp r TFA.Cmx
  Tmp   :: String -> Exp r t  -- dummy constructor
  Tag   :: String -> Exp r t -> Exp r t

deriving instance Show (Exp r t)

ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

instance Show (Exp r ta -> Exp r tb) where
  show f =

    let i = unsafePerformIO (do j <- readIORef ref
                                modifyIORef ref (+1)
                                return j)
        v = ("_x" ++ show i)
    in ("(\\ "++ v ++ " -> (" ++
        show (f (Tmp v))
        ++ "))")

instance Show (Env (Exp r) r') where
  show Emp        = "[]"
  show (Ext x xs) = "(" ++ show x ++ ") , " ++ show xs

eqlE :: Env (Exp r) r' -> Env (Exp r) r' -> Bool
eqlE Emp        Emp        = True
eqlE (Ext x xs) (Ext y ys) = eql x y && eqlE xs ys
eqlE _          _          = False

eql :: forall r t .  Exp r t -> Exp r t -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConB b)    (ConB b')     = b == b'
eql (ConF f)    (ConF f')     = f == f'
eql (AppV (v :: Var r tv) es)    (AppV (v' :: Var r tv') es') =
  case eqlSin (sin :: TFG.Typ tv) (sin :: TFG.Typ tv') of
    Rgt Rfl -> v == v' && eqlE es es'
    _       -> False
eql (Cnd ec et ef) (Cnd ec' et' ef') = eql ec ec' && eql et et' && eql ef ef'
eql (Whl ec eb ei) (Whl ec' eb' ei') = eqlF ec ec' && eqlF eb eb' && eql ei ei'
eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Fst (e :: Exp r (TFA.Tpl t ts))) (Fst (e' :: Exp r (TFA.Tpl t ts'))) =
  case eqlSin (sin :: TFG.Typ ts) (sin :: TFG.Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd (e :: Exp r (TFA.Tpl tf t))) (Snd (e' :: Exp r (TFA.Tpl tf' t))) =
  case eqlSin (sin :: TFG.Typ tf) (sin :: TFG.Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ary ei ef) (Ary ei' ef') = eql ei ei' && eqlF ef ef'
eql (Len (e :: Exp r (TFA.Ary ta))) (Len (e' :: Exp r (TFA.Ary ta'))) =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ind (e :: Exp r (TFA.Ary t)) ei) (Ind (e' :: Exp r (TFA.Ary t)) ei') =
    eql e e' && eql ei ei'
eql (Let (el :: Exp r ta) eb) (Let (el' :: Exp r ta') eb') =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql el el' && eqlF eb eb'
    _       -> False
eql (Cmx ei er) (Cmx ei' er') = eql ei ei' && eql er er'
eql (Tmp x    ) (Tmp x')      = x == x'
eql (Tag _ e)   e'            = eql e e'
eql e          (Tag _ e')     = eql e e'
eql _           _             = False

refEql :: IORef Int
{-# NOINLINE refEql #-}
refEql = unsafePerformIO (newIORef 0)

eqlF :: forall r ta tb.  (Exp r ta -> Exp r tb) -> (Exp r ta -> Exp r tb) -> Bool
eqlF f f' = let i = unsafePerformIO
                    (do j <- readIORef refEql
                        modifyIORef refEql (+1)
                        return j)
                v = "_x" ++ show i
            in eql (f (Tmp v)) (f' (Tmp v))

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc prd

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: (forall t'. Var r  t' -> Var r' t') ->
          (forall t'. Var r' t' -> Var r  t') ->
          Exp r t -> Exp r' t
mapVar _ _ (ConI i)       = ConI i
mapVar _ _ (ConB i)       = ConB i
mapVar _ _ (ConF i)       = ConF i
mapVar f g (AppV v es)    = AppV (f v) (ET.fmap (mapVar f g) es)
mapVar f g (Cnd ec et ef) = Cnd (mapVar f g ec) (mapVar f g et) (mapVar f g ef)
mapVar f g (Whl ec eb ei) = Whl (mapVar f g . ec . mapVar g f)
                                (mapVar f g . eb . mapVar g f) (mapVar f g ei)
mapVar f g (Tpl ef es)    = Tpl (mapVar f g ef) (mapVar f g es)
mapVar f g (Fst e)        = Fst (mapVar f g e)
mapVar f g (Snd e)        = Snd (mapVar f g e)
mapVar f g (Ary el ef)    = Ary (mapVar f g el) (mapVar f g . ef . mapVar g f)
mapVar f g (Len e)        = Len (mapVar f g e)
mapVar f g (Ind ea ei)    = Ind (mapVar f g ea) (mapVar f g ei)
mapVar f g (Let el eb)    = Let (mapVar f g el) (mapVar f g . eb . mapVar g f)
mapVar f g (Cmx er ei)    = Cmx (mapVar f g er) (mapVar f g ei)
mapVar _ _ (Tmp x)        = Tmp x
mapVar f g (Tag x e)      = Tag x (mapVar f g e)

absTmp :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) (absTmp xx s) es)
  Cnd ec et ef              -> Cnd (absTmp xx s ec)   (absTmp xx s et)
                                    (absTmp xx s ef)
  Whl ec eb ei              -> Whl (absTmp xx s . ec) (absTmp xx s . eb)
                                   (absTmp xx s ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> Tpl (absTmp xx s ef)   (absTmp xx s es)
  Fst e                     -> Fst (absTmp xx s e)
  Snd e                     -> Snd (absTmp xx s e)
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> Ary (absTmp xx s el)   (absTmp xx s . ef)
  Len e                     -> Len (absTmp xx s e)
  Ind ea ei                 -> Ind (absTmp xx s ea)   (absTmp xx s ei)
  Let el eb                 -> Let (absTmp xx s el)   (absTmp xx s . eb)
  Cmx er ei                 -> Cmx (absTmp xx s er)   (absTmp xx s ei)
  Tmp x
    | s == x                -> case eqlSin (sinTyp xx) t of
      Rgt Rfl               -> xx
      _                     -> ee
    | otherwise             -> ee
  Tag x e                   -> Tag x (absTmp xx s e)

-- when input string is not "__dummy__"
hasTmp :: String -> Exp r t -> Bool
hasTmp s ee = case ee of
  ConI _                    -> False
  ConB _                    -> False
  ConF _                    -> False
  AppV _ es                 -> foldl (\ b e -> b || hasTmp s e) False es
  Cnd ec et ef              -> hasTmp s ec || hasTmp s et || hasTmp s ef
  Whl ec eb ei              -> hasTmpF s ec || hasTmpF s eb || hasTmp s ei
  Tpl ef es                 -> hasTmp s ef || hasTmp s es
  Fst e                     -> hasTmp s e
  Snd e                     -> hasTmp s e
  Ary el ef                 -> hasTmp s el || hasTmpF s ef
  Len e                     -> hasTmp s e
  Ind ea ei                 -> hasTmp s ea || hasTmp s ei
  Let el eb                 -> hasTmp s el || hasTmpF s eb
  Cmx er ei                 -> hasTmp s er || hasTmp s ei
  Tmp x
    | s == x                -> True
    | otherwise             -> False
  Tag _ e                   -> hasTmp s e

hasTmpF :: String -> (Exp r ta -> Exp r tb) -> Bool
hasTmpF s f = hasTmp s (f (Tmp "__dummy__"))

isFresh :: (Exp r ta -> Exp r tb) -> Bool
isFresh f = not (hasTmp "__fresh__" (f (Tmp "__fresh__")))