module CSE where

import Expression.Feldspar.MiniWellScoped

import MyPrelude hiding (foldl)

import qualified Type.Feldspar.GADT     as TFG
import qualified Environment.Typed as ET
import ChangeMonad
import Data.IORef
import System.IO.Unsafe
import Singleton

cse :: forall r t. HasSin TFG.Typ t => Exp r t -> Exp r t
cse e = remTag (tilNotChg cseOne e)


reff :: IORef Int
{-# NOINLINE reff #-}
reff = unsafePerformIO (newIORef 0)

cseF :: forall r a b. (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           (Exp r a -> Exp r b) -> (Exp r a -> Exp r b)
cseF f  = let i = unsafePerformIO (do j <- readIORef reff
                                      modifyIORef ref (+1)
                                      return j)
              v = "_xn" ++ show i
          in remTag . (\ x -> absTmp x v (cse (f (Tmp v))))


remTag :: Exp r t -> Exp r t
remTag ee = case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) remTag es)
  Cnd ec et ef              -> Cnd (remTag ec) (remTag et) (remTag ef)
  Whl ec eb ei              -> Whl (remTag . ec) (remTag . eb) (remTag ei)
  Tpl ef es                 -> Tpl (remTag ef)   (remTag es)
  Fst e                     -> Fst (remTag e)
  Snd e                     -> Snd (remTag e)
  Ary el ef                 -> Ary (remTag el)   (remTag . ef)
  Len e                     -> Len (remTag e)
  Ind ea ei                 -> Ind (remTag ea)   (remTag ei)
  Let el eb                 -> Let (remTag el)   (remTag . eb)
  Cmx er ei                 -> Cmx (remTag er)   (remTag ei)
  Tmp x                     -> Tmp x
  Tag _ e                   -> remTag e


cseOne :: forall r t. HasSin TFG.Typ t => Exp r t -> Chg (Exp r t)
cseOne ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> pure (ConI i)
  ConB b                    -> pure (ConB b)
  ConF f                    -> pure (ConF f)
  AppV v es                 -> AppV v <$> (TFG.mapMC (sinTyp v) cseOne es)
  Cnd ec et ef              -> case findTag ec of
    Just (x , Exs1 ex tx) | hasTag x et || hasTag x ef -> case getPrfHasSin tx of
      PrfHasSin ->
          chg (Let ex (\ xx -> Cnd (absTag xx x ec) (absTag xx x et)
                               (absTag xx x ef)))
    _ -> case findTag et of
     Just (x , Exs1 ex tx) | hasTag x ef -> case getPrfHasSin tx of
      PrfHasSin ->
          chg (Let ex (\ xx -> Cnd ec (absTag xx x et) (absTag xx x ef)))
     _ -> Cnd <$> cseOne ec <*> cseOne et <*> cseOne ef
  Whl ec eb ei              -> case findTagF ec of
    Just (x , Exs1 ex tx) | hasTagF x eb
                            || hasTag x ei -> case getPrfHasSin tx of
      PrfHasSin ->
          chg (Let ex (\ xx -> Whl (absTag xx x . ec) (absTag xx x . eb)
                               (absTag xx x ei)))
    _ -> case findTagF eb of
     Just (x , Exs1 ex tx) | hasTag x ei -> case getPrfHasSin tx of
      PrfHasSin ->
          chg (Let ex (\ xx -> Whl ec (absTag xx x . eb) (absTag xx x ei)))
     _ -> Whl <$> cseOneF ec <*> cseOneF eb <*> cseOne ei
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> case findTag ef of
      Just (x , Exs1 ex tx) | hasTag x es -> case getPrfHasSin tx of
        PrfHasSin ->
          chg (Let ex (\ xx -> Tpl (absTag xx x ef) (absTag xx x es)))
      _ -> Tpl <$> cseOne ef <*> cseOne es
  Fst e                     -> Fst <$> cseOne e
  Snd e                     -> Snd <$> cseOne e
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> case findTag el of
      Just (x , Exs1 ex tx) | hasTagF x ef -> case getPrfHasSin tx of
        PrfHasSin ->
          chg (Let ex (\ xx -> Ary (absTag xx x el) (absTag xx x . ef)))
      _ -> Ary <$> cseOne el <*> cseOneF ef
  Len e                     -> Len <$> cseOne e
  Ind ea ei                 -> case findTag ea of
      Just (x , Exs1 ex tx) | hasTag x ei -> case getPrfHasSin tx of
        PrfHasSin ->
          chg (Let ex (\ xx -> Ind (absTag xx x ea) (absTag xx x ei)))
      _ -> Ind <$> cseOne ea <*> cseOne ei
  Let el eb                 -> case findTag el of
      Just (x , Exs1 ex tx) | hasTagF x eb -> case getPrfHasSin tx of
        PrfHasSin ->
          chg (Let ex (\ xx -> Let (absTag xx x el) (absTag xx x . eb)))
      _ -> Let <$> cseOne el <*> cseOneF eb
  Cmx er ei                 -> case findTag er of
      Just (x , Exs1 ex tx) | hasTag x ei -> case getPrfHasSin tx of
        PrfHasSin ->
          chg (Let ex (\ xx -> Cmx (absTag xx x er) (absTag xx x ei)))
      _ -> Cmx <$> cseOne er <*> cseOne ei

  Tmp x                     -> pure (Tmp x)
  Tag x e                   -> Tag x <$> cseOne e

ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

cseOneF :: forall r a b. (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           (Exp r a -> Exp r b) -> Chg (Exp r a -> Exp r b)
cseOneF f  = let i = unsafePerformIO (do j <- readIORef ref
                                         modifyIORef ref (+1)
                                         return j)
                 v = "_xn" ++ show i
             in do eb <- cseOne (f (Tmp v))
                   return (\ x -> absTmp x v eb)

infixl 4 <||>

(<||>) :: Maybe a -> Maybe a -> Maybe a
Nothing  <||> m = m
(Just x) <||> _ = Just x

fld :: (forall t. HasSin TFG.Typ t => b -> e t -> b) -> b ->
       TFG.Typ tt -> ET.Env e (TFG.Arg tt) -> b
fld _ z _              ET.Emp        = z
fld f z (TFG.Arr a b) (ET.Ext e es)  = case getPrfHasSin a of
    PrfHasSin -> f (fld f z b es) e
fld _ _  _             _             = impossible


findTag :: forall r t. HasSin TFG.Typ t =>
          Exp r t -> Maybe (String , Exs1 (Exp r) TFG.Typ)
findTag ee = let t = sin :: TFG.Typ t in case ee of
  ConI _                    -> Nothing
  ConB _                    -> Nothing
  ConF _                    -> Nothing
  AppV v es                 -> fld (\ b e -> b <||> (findTag e)) Nothing
                               (sinTyp v) es
  Cnd ec et ef              -> findTag  ec <||> findTag  et <||> findTag ef
  Whl ec eb ei              -> findTagF ec <||> findTagF eb <||> findTag ei
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> findTag  ef <||> findTag  es
  Fst e                     -> findTag  e
  Snd e                     -> findTag  e
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> findTag  el <||> findTagF ef
  Len e                     -> findTag  e
  Ind ea ei                 -> findTag  ea <||> findTag  ei
  Let el eb                 -> findTag  el <||> findTagF eb
  Cmx er ei                 -> findTag  er <||> findTag  ei
  Tmp _                     -> Nothing
  Tag x e                   -> Just (x , Exs1 e (sinTyp e))

findTagF :: (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
            (Exp r a -> Exp r b) -> Maybe (String , Exs1 (Exp r) TFG.Typ)
findTagF f = findTag (f (Tmp "dummy"))


absTag :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTag xx s ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) (absTag xx s) es)
  Cnd ec et ef              -> Cnd (absTag xx s ec)   (absTag xx s et)
                                    (absTag xx s ef)
  Whl ec eb ei              -> Whl (absTag xx s . ec) (absTag xx s . eb)
                                   (absTag xx s ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> Tpl (absTag xx s ef)   (absTag xx s es)
  Fst e                     -> Fst (absTag xx s e)
  Snd e                     -> Snd (absTag xx s e)
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> Ary (absTag xx s el)   (absTag xx s . ef)
  Len e                     -> Len (absTag xx s e)
  Ind ea ei                 -> Ind (absTag xx s ea)   (absTag xx s ei)
  Let el eb                 -> Let (absTag xx s el)   (absTag xx s . eb)
  Cmx er ei                 -> Cmx (absTag xx s er)   (absTag xx s ei)
  Tmp x                     -> Tmp x
  Tag x _
    | s == x                -> case eqlSin (sinTyp xx) t of
      Rgt Rfl               -> xx
      _                     -> ee
    | otherwise             -> ee

hasTag :: String -> Exp r t -> Bool
hasTag s ee = case ee of
  ConI _                    -> False
  ConB _                    -> False
  ConF _                    -> False
  AppV _ es                 -> ET.foldl (\ b e -> b || hasTag s e) False es
  Cnd ec et ef              -> hasTag s ec || hasTag s et || hasTag s ef
  Whl ec eb ei              -> hasTagF s ec || hasTagF s eb || hasTag s ei
  Tpl ef es                 -> hasTag s ef || hasTag s es
  Fst e                     -> hasTag s e
  Snd e                     -> hasTag s e
  Ary el ef                 -> hasTag s el || hasTagF s ef
  Len e                     -> hasTag s e
  Ind ea ei                 -> hasTag s ea || hasTag s ei
  Let el eb                 -> hasTag s el || hasTagF s eb
  Cmx er ei                 -> hasTag s er || hasTag s ei
  Tmp _                     -> False
  Tag x e
    | s == x                -> True
    | otherwise             -> hasTag s e

hasTagF :: String -> (Exp r ta -> Exp r tb) -> Bool
hasTagF s f = hasTag s (f (Tmp "__dummy__"))
