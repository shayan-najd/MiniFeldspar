module Examples.Feldspar.Prelude.MiniWellScoped 
       (Data 
       ,Integer,Integral(..),litI
       ,Float,Rational(..),litF
       ,Bool,pattern True, pattern False
       ,Tpl,tpl,fst,snd           
       ,Complex,cmx,real,imag
       ,Vec,vec,lenV,indV           
       ,Ary,ary,len,ind               
       ,ifThenElse,whl,forLoop,forLoopVec --memorize
       ,not,and,or                  
       ,Equality(eql),notEql
       ,Ordering(lt),gt,lte,gte,min           
       ,Numeric(add,sub,mul,div,neg),ilog2 --sqrt
       ,bitXor,bitAnd,bitOr,shfRgt,shfLft,complement,testBit,lsbs,oneBits
       ,i2f,cis,ary2vec,vec2ary                                  
       ,frmTo,permute,reverse,foldl,foldlVec,map,zipWith,sum,scalarProd,fromList
       ,frmToA,permuteA,reverseA,foldlA,mapA,zipWithA,sumA,scalarProdA,fromListA
       ) where

import qualified Prelude   as P
import qualified MyPrelude as MP

import Expression.Feldspar.MiniWellScoped
import qualified Type.Feldspar.ADT             as TFA
import qualified Type.Feldspar.GADT            as TFG

import Singleton
import Environment.Typed (Env(Emp,Ext))
import Examples.Feldspar.Prelude.Environment

type Type t = HasSin TFG.Typ t
 
type Data t = Exp Prelude t

type Ary t = TFA.Ary t

---------------------------------------------------------------------------------
-- Integer
---------------------------------------------------------------------------------

type Integer = TFA.Int
  
class Integral t where               
  fromInteger :: P.Integer -> t
 
instance Integral MP.Int where
  fromInteger x = MP.fromIntegral x 

instance Integral MP.Integer where
  fromInteger x = MP.fromIntegral x 

instance Integral (Data Integer) where
  fromInteger = litI MP.. MP.fromIntegral
 
litI :: MP.Integer -> Data Integer
litI = ConI

---------------------------------------------------------------------------------
-- Float
---------------------------------------------------------------------------------
  
type Float   = TFA.Flt

class Rational t where
  fromRational :: P.Rational -> t
 
instance Rational MP.Float where
  fromRational x = MP.fromRational x

instance Rational (Data Float) where
  fromRational = litF MP.. MP.fromRational

litF :: MP.Float -> Data Float 
litF = ConF

---------------------------------------------------------------------------------
-- Bool
---------------------------------------------------------------------------------

type Bool = TFA.Bol  

pattern True  = ConB MP.True

pattern False = ConB MP.False 
 
---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

type Tpl a b = TFA.Tpl a b

tpl :: Data a -> Data b -> Data (Tpl a b)
tpl = Tpl

fst :: Type b => Data (Tpl a b) -> Data a
fst = Fst

snd :: Type a => Data (Tpl a b) -> Data b
snd = Snd

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

type Complex = TFA.Cmx

cmx :: Data Float -> Data Float -> Data Complex 
cmx = Cmx

real :: Data Complex -> Data Float
real e = AppV realPartVar (Ext e Emp) 

imag :: Data Complex -> Data Float
imag e = AppV imagPartVar (Ext e Emp)

---------------------------------------------------------------------------------
-- Vec
---------------------------------------------------------------------------------

data Vec t = Vec (Data Integer) (Data Integer -> Data t)
 
vec :: Data Integer -> (Data Integer -> Data t) -> Vec t
vec = Vec

lenV :: Vec t -> Data Integer
lenV (Vec l _) = l

indV :: Vec t -> Data Integer -> Data t
indV (Vec _ f) = f 

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

ary :: Data Integer -> (Data Integer -> Data t) -> Data (Ary t)
ary = Ary

len  :: Type t => Data (Ary t) -> Data Integer
len = Len

ind :: Data (Ary t) -> Data Integer -> Data t
ind = Ind 

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

ifThenElse :: Data Bool -> Data a -> Data a -> Data a
ifThenElse = Cnd

whl :: (Data t -> Data Bool) -> (Data t -> Data t) -> Data t -> Data t  
whl = Whl

forLoop :: Type s => Data Integer -> Data s -> 
           (Data Integer -> Data s -> Data s ) -> Data s
forLoop l init step = Snd (Whl (\ t -> lt (Fst t) l)
                               (\ t -> Tpl 
                                       (add  (Fst t) (ConI 1)) 
                                       (step (Fst t) (Snd t)))
                               (Tpl (ConI 0) init))                      

forLoopVec :: Type s => Data Integer -> Vec s -> 
           (Data Integer -> Vec s -> Vec s) -> Vec s
forLoopVec l init step =  let init'     = vec2ary init
                              step' i a = vec2ary (step i (ary2vec a))
                          in  ary2vec (forLoop l init' step')    

---------------------------------------------------------------------------------
-- Boolean Operators
---------------------------------------------------------------------------------

not :: Data Bool -> Data Bool
not x = ifThenElse x False True

and :: Data Bool -> Data Bool -> Data Bool
and x y = ifThenElse x y False

or :: Data Bool -> Data Bool -> Data Bool
or x y = ifThenElse x True y

---------------------------------------------------------------------------------
-- Equality
---------------------------------------------------------------------------------
  
class Equality t where
  eql :: Data t -> Data t -> Data Bool
  
instance Equality Bool where
  eql el er = AppV eqlBolVar (Ext el (Ext er Emp)) 

instance Equality Integer where
  eql el er = AppV eqlIntVar (Ext el (Ext er Emp)) 

instance Equality Float where
  eql el er = AppV eqlFltVar (Ext el (Ext er Emp)) 
         
notEql :: Equality t => Data t -> Data t -> Data Bool 
notEql x y = not (eql x y) 

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  lt :: Data t -> Data t -> Data Bool
   
instance Ordering Bool where
  lt el er = AppV ltdBolVar (Ext el (Ext er Emp))  

instance Ordering Integer where
  lt el er = AppV ltdIntVar (Ext el (Ext er Emp))  
  
instance Ordering Float where
  lt el er = AppV ltdFltVar (Ext el (Ext er Emp))  
    
gt :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
gt x y = not (or (lt x y) (eql x y)) 

lte :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
lte x y = or (lt x y) (eql x y)

gte :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
gte x y = not (lt x y)

min :: Ordering t => Data t -> Data t -> Data t
min x y = ifThenElse (lt x y) x y

---------------------------------------------------------------------------------
-- Numeric
---------------------------------------------------------------------------------

class Numeric t where
  add :: Data t -> Data t -> Data t
  sub :: Data t -> Data t -> Data t
  mul :: Data t -> Data t -> Data t
  div :: Data t -> Data t -> Data t
  neg :: Data t -> Data t
 
instance Numeric Integer where
  add el er = AppV addIntVar (Ext el (Ext er Emp))  
  sub el er = AppV subIntVar (Ext el (Ext er Emp))  
  mul el er = AppV mulIntVar (Ext el (Ext er Emp))  
  div el er = AppV divIntVar (Ext el (Ext er Emp))  
  neg       = sub (litI 0)
   
instance Numeric Float where 
  add el er = AppV addFltVar (Ext el (Ext er Emp))  
  sub el er = AppV subFltVar (Ext el (Ext er Emp))  
  mul el er = AppV mulFltVar (Ext el (Ext er Emp))  
  div el er = AppV divFltVar (Ext el (Ext er Emp))  
  neg       = sub (litF 0)

instance Numeric (Complex) where 
  add el er = AppV addCmxVar (Ext el (Ext er Emp))  
  sub el er = AppV subCmxVar (Ext el (Ext er Emp))  
  mul el er = AppV mulCmxVar (Ext el (Ext er Emp))  
  div el er = AppV divCmxVar (Ext el (Ext er Emp))  
  neg       = sub (cmx (litF 0.0) (litF 0.0))
  
ilog2 :: Data Integer -> Data Integer
ilog2 xx = AppV ilog2Var (Ext xx Emp)
  {-
  (ConI 31) - nlz xx
 where 
   nlz :: Data Integer -> Data Integer
   nlz x = bitCount (complement (part x))
   
   part :: Data Integer -> Data Integer
   part x = MP.foldl go x [1,2,4,8,16] 
       where 
         go :: Data Integer -> MP.Integer -> Data Integer
         go b s = b .|. (b .>>. (litI s))
  -}
---------------------------------------------------------------------------------
-- Bitwise Operators
---------------------------------------------------------------------------------

bitXor        :: Data Integer -> Data Integer -> Data Integer
bitXor el er  = AppV xorIntVar (Ext el (Ext er Emp))  

bitAnd      :: Data Integer -> Data Integer -> Data Integer 
bitAnd el er  = AppV andIntVar (Ext el (Ext er Emp))  

bitOr      :: Data Integer -> Data Integer -> Data Integer
bitOr  el er  = AppV orIntVar (Ext el (Ext er Emp))  

shfRgt     :: Data Integer -> Data Integer -> Data Integer
shfRgt el er  = AppV shrIntVar (Ext el (Ext er Emp))  

shfLft     :: Data Integer -> Data Integer -> Data Integer
shfLft el er  = AppV shlIntVar (Ext el (Ext er Emp))  
 
complement :: Data Integer -> Data Integer
complement e  = AppV cmpIntVar (Ext e Emp)

testBit    :: Data Integer -> Data Integer -> Data Bool 
testBit el er =  ifThenElse (eql (bitAnd el (shfLft (ConI 1) er)) (ConI 0)) 
                 False  
                 True
 
oneBits :: Data Integer -> Data Integer
oneBits n = complement (shfLft (complement (ConI 0)) n)

lsbs :: Data Integer -> Data Integer -> Data Integer
lsbs k i = bitAnd i (oneBits k)

---------------------------------------------------------------------------------
-- Conversion Operators
---------------------------------------------------------------------------------
 
i2f :: Data Integer -> Data Float
i2f e = AppV i2fVar (Ext e Emp)

cis :: Data Float -> Data (Complex)
cis e = AppV cisVar (Ext e Emp)
 
vec2ary :: Vec t -> Data (Ary t)
vec2ary v = Ary (lenV v) (indV v) 

ary2vec :: Type t => Data (Ary t) -> Vec t
ary2vec v = vec (Len v) (\i -> Ind v i)

---------------------------------------------------------------------------------
-- Vector Operators
---------------------------------------------------------------------------------

frmTo :: Data Integer -> Data Integer -> Vec Integer
frmTo m n = vec 
            (ifThenElse (lt n m) 
             (ConI 0) 
             (add (sub n m) (ConI 1))) 
            (\ i -> add i m)
  
permute :: (Data Integer -> Data Integer -> Data Integer)
           -> Vec t -> Vec t
permute f v = vec (lenV v) (\ i -> indV v (f (lenV v) i))
 
reverse :: Vec t -> Vec t
reverse = permute (\ l i -> sub (sub l (ConI 1)) i)

foldl :: Type a => 
         (Data a -> Data b -> Data a) -> Data a -> Vec b -> Data a
foldl f acc v  = forLoop (lenV v) acc (\ i a ->  f a (indV v i))
   

foldlVec :: Type a => 
         (Vec a -> Data b -> Vec a) -> Vec a -> 
         Vec b -> Vec a
foldlVec f acc v  = let acc' = vec2ary acc
                        f' vv d = vec2ary (f (ary2vec vv) d)
                    in  ary2vec (forLoop (lenV v) acc' (\ i a ->  f' a (indV v i)))

map :: (Data a -> Data b) -> Vec a -> Vec b
map f v = vec (lenV v) (\i -> f (indV v i))     

zipWith :: (Data a -> Data b -> Data c) -> Vec a -> Vec b -> Vec c
zipWith f v1 v2 = vec (min (lenV v1) (lenV v2))
                      (\ i -> f (indV v1 i) (indV v2 i))

sum :: Vec Integer -> Data Integer
sum = foldl add (ConI 0)

scalarProd :: Vec Integer -> Vec Integer -> Data Integer
scalarProd v1 v2 = sum (zipWith mul v1 v2)

---------------------------------------------------------------------------------
-- Ary Operators
---------------------------------------------------------------------------------

frmToA :: Data Integer -> Data Integer -> Data (Ary Integer)
frmToA m n = Ary 
             (ifThenElse (lt n m) 
              (ConI 0) 
              (add (sub n m) (ConI 1)))
             (\ i -> add i m)
  
permuteA :: Type t => 
            (Data Integer -> Data Integer -> Data Integer)
           -> Data (Ary t) -> Data (Ary t)
permuteA f v = Ary (len v) (\ i -> ind v (f (len v) i))
 
reverseA :: Type t => Data (Ary t) -> Data (Ary t)
reverseA = permuteA (\ l i -> sub (sub l (ConI 1)) i)

foldlA :: (Type a , Type b) => 
         (Data a -> Data b -> Data a) -> Data a -> Data (Ary b) -> Data a
foldlA f acc v  = forLoop (len v) acc (\ i a -> f a (ind v i))
   
mapA :: Type a => 
        (Data a -> Data b) -> Data (Ary a) -> Data (Ary b)
mapA f v = Ary (len v) (\ i -> f (ind v i))     

zipWithA :: (Type a , Type b) => 
            (Data a -> Data b -> Data c) -> Data (Ary a) -> Data (Ary b) -> 
           Data (Ary c)
zipWithA f v1 v2 = Ary (min (len v1) (len v2))
                      (\ i -> f (ind v1 i) (ind v2 i))

sumA :: Data (Ary Integer) -> Data Integer
sumA = foldlA add (ConI 0)

scalarProdA :: Data (Ary Integer) -> Data (Ary Integer) -> Data Integer
scalarProdA v1 v2 = sumA (zipWithA mul v1 v2)

---------------------------------------------------------------------------------
-- Helper Operators
---------------------------------------------------------------------------------

fromList:: [Data a] -> Data a -> Vec a
fromList lst k =  vec
                  (litI (MP.fromIntegral (MP.length lst))) 
                  (\ i ->  MP.foldr 
                           (\ j acc -> ifThenElse 
                                       (eql i 
                                        (litI (MP.fromIntegral j)))
                                       (lst MP.!! j)
                                       acc)
                           k         
                           (MP.enumFromTo 0 (MP.length lst MP.- 1)))
                  
fromListA :: [Data a] -> Data a -> Data (Ary a)
fromListA lst k = ary
                  (litI (MP.fromIntegral (MP.length lst))) 
                  (\ i ->  MP.foldr 
                           (\ j acc -> ifThenElse 
                                       (eql i 
                                        (litI (MP.fromIntegral j)))
                                       (lst MP.!! j)
                                       acc)
                           k         
                           (MP.enumFromTo 0 (MP.length lst MP.- 1)))