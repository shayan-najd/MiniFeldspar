{-# LANGUAGE GADTs, ExistentialQuantification, PatternGuards, NoMonomorphismRestriction #-}
module TExp where
import Data.Maybe
import Control.Monad
import UExp

data TFun a where
    TBody :: TExp a                 -> TFun a
    TLam  :: Id -> TTyp a -> TFun b -> TFun (a->b)

data TTyp a where
    TTBol ::                     TTyp Bool
    TTDbl ::                     TTyp Double
    TTArr :: TTyp a -> TTyp b -> TTyp (a->b)

data TExp a where
    TDbl   :: Double                                            -> TExp Double
    TBol   :: Bool                                              -> TExp Bool
    TDblOp :: DblOp     -> TExp Double -> TExp Double           -> TExp Double
    TBolOp :: BolOp     -> TExp Bool   -> TExp Bool             -> TExp Bool
    TCmpOp :: CmpOp     -> TExp Double -> TExp Double           -> TExp Bool
    TIf    :: TExp Bool -> TExp a      -> TExp a                -> TExp a
    TLet   :: Id        -> TTyp a      -> TExp a      -> TExp b -> TExp b
    TVar   :: Id                                                -> TExp a

data DblOp = DAdd | DSub | DMul | DDiv
    deriving (Eq, Show)

data BolOp = BAnd | BOr
    deriving (Eq, Show)

data CmpOp = CEq | CLe
    deriving (Eq, Show)
             
instance Show (TFun a) where
    showsPrec p (TBody e) = showsPrec p e
    showsPrec p (TLam i t e) = showParen (p>0)
      (showString "\\ " . showParen True (showString i . showString " :: " . showsPrec 0 t) . showString " -> " . showsPrec 0 e)

instance Show (TTyp a) where
    showsPrec _ TTBol = showString "Bool"
    showsPrec _ TTDbl = showString "Double"
    showsPrec p (TTArr a b) = showParen (p>5) (showsPrec 6 a . showString " -> " . showsPrec 5 b)

instance Show (TExp a) where
    showsPrec p (TDbl d) = showsPrec p d
    showsPrec p (TBol b) = showsPrec p b
    showsPrec _ (TVar i) = showString i
    showsPrec p (TDblOp op a b) = showOp p 
                                  (fromJust $ lookup op 
                                   [(DMul, "*"), (DAdd, "+")
                                   , (DSub, "-"), (DDiv, "/")]) a b
    showsPrec p (TBolOp op a b) = showOp p (fromJust $ lookup op 
                                            [(BAnd, "&&"), (BOr, "||")]) a b
    showsPrec p (TCmpOp op a b) = showOp p (fromJust $ lookup op 
                                            [(CEq, "=="), (CLe, "<=")]) a b
    showsPrec p (TIf c t e) = showParen (p>0) (showString "if " . showsPrec 0 c 
                                               . showString " then " 
                                               . showsPrec 0 t 
                                               . showString " else " 
                                               . showsPrec 0 e)
    showsPrec p (TLet i _ e b) = showParen (p>0) (showString "let " 
                                                  . showString i 
                                                  . showString " = " 
                                                  . showsPrec 0 e 
                                                  . showString " in " 
                                                  . showsPrec 0 b)             
                                 
                                 
data ATExp = forall a . TExp a ::: TTyp a

data AFun = forall a . AFun (TFun a) (TTyp a)                                

instance Show ATExp where
  show (x ::: ty) = show x ++ " ::: " ++ show ty

instance Show AFun where
  show (AFun tf ty) = show tf ++ " :: " ++ show ty

data Equal a b where
    Eq :: Equal a a
    
test :: TTyp a -> TTyp b -> Maybe (Equal a b)
test TTBol TTBol = return Eq
test TTDbl TTDbl = return Eq
test (TTArr a b) (TTArr a' b') = do
    Eq <- test a a'
    Eq <- test b b'
    return Eq
test _ _ = mzero    
 
type Env = [(Id, ATExp)]

typeCheckExp :: Env -> UExp -> Maybe ATExp
typeCheckExp _ (UDbl d) =
    return $ TDbl d ::: TTDbl
typeCheckExp _ (UBol b) =
    return $ TBol b ::: TTBol
typeCheckExp r (UApp op [a, b]) | Just dop <- lookup op [("+", DAdd), ("-", DSub), ("*", DMul), ("/", DDiv)] = do
    a' ::: TTDbl <- typeCheckExp r a
    b' ::: TTDbl <- typeCheckExp r b
    return $ TDblOp dop a' b' ::: TTDbl
typeCheckExp r (UApp op [a, b]) | Just bop <- lookup op [("&&", BAnd), ("||", BOr)] = do
    a' ::: TTBol <- typeCheckExp r a
    b' ::: TTBol <- typeCheckExp r b
    return $ TBolOp bop a' b' ::: TTBol
typeCheckExp r (UApp op [a, b]) | Just cop <- lookup op [("==", CEq), ("<=", CLe)] = do
    a' ::: TTDbl <- typeCheckExp r a
    b' ::: TTDbl <- typeCheckExp r b
    return $ TCmpOp cop a' b' ::: TTBol
typeCheckExp r (UApp "if" [c,t,e]) = do
    c' ::: TTBol <- typeCheckExp r c
    t' ::: tt    <- typeCheckExp r t
    e' ::: te    <- typeCheckExp r e
    Eq <- test tt te
    return $ TIf c' t' e' ::: tt
typeCheckExp r (ULet i e b) = do
    e' ::: te <- typeCheckExp r e
    b' ::: tb <- typeCheckExp ((i, TVar i ::: te) : r) b
    return $ TLet i te e' b' ::: tb
typeCheckExp r (UVar i) =
    lookup i r
typeCheckExp _ _ =
    mzero


typeCheck :: UFun -> Maybe AFun
typeCheck = typeCheckFun []

typeCheckFun :: Env -> UFun -> Maybe AFun
typeCheckFun n (UFun [] b) = do
    e ::: t <- typeCheckExp n b
    return $ AFun (TBody e) t
typeCheckFun n (UFun ((x, typ):vts) b) =
  case typ of
    UTBol -> f TTBol
    UTDbl -> f TTDbl
  where 
    f :: TTyp a -> Maybe AFun
    f t = do 
          AFun e r <- typeCheckFun ((x, TVar x ::: t) : n) 
                      (UFun vts b); return $ AFun (TLam x t e) (TTArr t r)
                                    
                                    
class Type a where
    theType :: TTyp a
instance Type Double where
    theType = TTDbl
instance Type Bool where
    theType = TTBol
instance (Type a, Type b) => Type (a->b) where
    theType = TTArr theType theType

extractFun :: (Type a) => AFun -> Maybe (TFun a)
extractFun = extract theType

extract :: TTyp a -> AFun -> Maybe (TFun a)
extract s (AFun e t) = do
    Eq <- test t s
    return e                                    
    
toTFun :: (Type a) => UFun -> Maybe (TFun a)
toTFun = extractFun <=< typeCheck    

example = print (mParseUFun "\\ (x::Double) -> x+1" >>= typeCheck)