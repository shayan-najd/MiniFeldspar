module Examples.Feldspar.Prelude.MiniWellScoped 
       (Data 
       ,Integer,Integral(..),litI
       ,Float,Rational(..),litF
       ,Bool,true,false
       ,Tpl,tpl,fst,snd           
       ,Complex,cmx,real,imag
       ,Vec,vec,length,(!!)           
       ,Ary,ary, ind , len              
       ,ifThenElse,forLoop,forLoopVec,whl
       ,not,(&&),(||)                  
       ,Equality((==)),(/=)
       ,Ordering((<)),(>),(>=),(<=),min           
       ,Numeric((+),(-),(*),(/),negate),ilog2
       ,xor,(.&.),(.|.),(.>>.),(.<<.),complement,testBit,lsbs,oneBits
       ,i2f,cis,ary2vec,vec2ary                                  
       ,(...),permute,reverse,foldl,foldlVec,map,zipWith,sum,scalarProd,fromList
       ,(....),permuteA,reverseA,foldlA,mapA,zipWithA,sumA,scalarProdA,fromListA
       ) where

import Prelude ()
import qualified Prelude   as P
import qualified MyPrelude as MP

import Expression.Feldspar.MiniWellScoped
import qualified Type.Feldspar.ADT             as TFA
import qualified Type.Feldspar.GADT            as TFG

import Singleton
import Environment.Typed hiding (map,len)
import Examples.Feldspar.Prelude.Environment
 
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

true :: Data Bool
true = ConB MP.True

false :: Data Bool
false = ConB MP.False

---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

type Tpl a b = TFA.Tpl a b

tpl :: Data a -> Data b -> Data (Tpl a b)
tpl = Tpl

fst :: HasSin TFG.Typ b => Data (Tpl a b) -> Data a
fst = Fst

snd :: HasSin TFG.Typ a => Data (Tpl a b) -> Data b
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

data Vec t = Vec (Data Integer) (Data Integer -> t)
 
vec :: Data Integer -> (Data Integer -> t) -> Vec t
vec = Vec

length :: Vec t -> Data Integer
length (Vec l _) = l

(!!) :: Vec t -> Data Integer -> t
(!!) (Vec _ f) = f 

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

ary :: Data Integer -> (Data Integer -> Data t) -> Data (Ary t)
ary = Ary

len  :: HasSin TFG.Typ t => Data (Ary t) -> Data Integer
len = Len

lengthA = len 

ind :: Data (Ary t) -> Data Integer -> Data t
ind = Ind 

(!!!) = ind
---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

ifThenElse :: Data Bool -> Data a -> Data a -> Data a
ifThenElse = Cnd

whl :: (Exp r t -> Exp r Bool) -> (Exp r t -> Exp r t) -> Exp r t -> Exp r t  
whl = Whl

forLoop :: HasSin TFG.Typ s => Data Integer -> Data s -> 
           (Data Integer -> Data s -> Data s ) -> Data s
forLoop l init step = Snd (Whl (\ t -> (Fst t) < l)
                               (\ t -> Tpl 
                                       ((Fst t) + (ConI 1)) 
                                       (step (Fst t) (Snd t)))
                               (Tpl (ConI 0) init))
                      

forLoopVec :: HasSin TFG.Typ s => Data Integer -> Vec (Data s) -> 
           (Data Integer -> Vec (Data s) -> Vec (Data s)) -> Vec (Data s)
forLoopVec l init step =  let init'     = vec2ary init
                              step' i a = vec2ary (step i (ary2vec a))
                          in  ary2vec (forLoop l init' step')    

---------------------------------------------------------------------------------
-- Boolean Operators
---------------------------------------------------------------------------------

not :: Data Bool -> Data Bool
not x = ifThenElse x false true

(&&) :: Data Bool -> Data Bool -> Data Bool
x && y = ifThenElse x y false

(||) :: Data Bool -> Data Bool -> Data Bool
x || y = ifThenElse x true y

---------------------------------------------------------------------------------
-- Equality
---------------------------------------------------------------------------------
  
class Equality t where
  (==) :: Data t -> Data t -> Data Bool
  
instance Equality Bool where
  el == er = AppV eqlBolVar (Ext el (Ext er Emp)) 

instance Equality Integer where
  el == er = AppV eqlIntVar (Ext el (Ext er Emp)) 

instance Equality Float where
  el == er = AppV eqlFltVar (Ext el (Ext er Emp)) 
         
(/=) :: Equality t => Data t -> Data t -> Data Bool 
x /= y = not (x == y) 

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  (<) :: Data t -> Data t -> Data Bool
   
instance Ordering Bool where
  el < er = AppV ltdBolVar (Ext el (Ext er Emp))  

instance Ordering Integer where
  el < er = AppV ltdIntVar (Ext el (Ext er Emp))  
  
instance Ordering Float where
  el < er = AppV ltdFltVar (Ext el (Ext er Emp))  
    
(>) :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
x > y = not ((x < y) || (x == y)) 

(>=) :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
x >= y = not (x < y)

(<=) :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
x <= y = (x < y) || (x == y)

min :: Ordering t => Data t -> Data t -> Data t
min x y = ifThenElse (x < y) x y

---------------------------------------------------------------------------------
-- Numeric
---------------------------------------------------------------------------------

class Numeric t where
  (+)    :: Data t -> Data t -> Data t
  (-)    :: Data t -> Data t -> Data t
  (*)    :: Data t -> Data t -> Data t
  (/)    :: Data t -> Data t -> Data t
  negate :: Data t -> Data t
 
instance Numeric Integer where
  el + er = AppV addIntVar (Ext el (Ext er Emp))  
  el - er = AppV subIntVar (Ext el (Ext er Emp))  
  el * er = AppV mulIntVar (Ext el (Ext er Emp))  
  el / er = AppV divIntVar (Ext el (Ext er Emp))  
  negate  = (litI 0 -)
   
instance Numeric Float where 
  el + er = AppV addFltVar (Ext el (Ext er Emp))  
  el - er = AppV subFltVar (Ext el (Ext er Emp))  
  el * er = AppV mulFltVar (Ext el (Ext er Emp))  
  el / er = AppV divFltVar (Ext el (Ext er Emp))  
  negate  = (litF 0 -)

instance Numeric (Complex) where 
  el + er = AppV addCmxVar (Ext el (Ext er Emp))  
  el - er = AppV subCmxVar (Ext el (Ext er Emp))  
  el * er = AppV mulCmxVar (Ext el (Ext er Emp))  
  el / er = AppV divCmxVar (Ext el (Ext er Emp))  
  negate  = (cmx (litF 0.0) (litF 0.0) -)
  
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

(.&.)      :: Data Integer -> Data Integer -> Data Integer 
el .&. er     = AppV andIntVar (Ext el (Ext er Emp))  

(.|.)      :: Data Integer -> Data Integer -> Data Integer
el .|. er     = AppV orIntVar (Ext el (Ext er Emp))  

xor        :: Data Integer -> Data Integer -> Data Integer
xor el er     = AppV xorIntVar (Ext el (Ext er Emp))  

(.>>.)     :: Data Integer -> Data Integer -> Data Integer
el .>>. er    = AppV shrIntVar (Ext el (Ext er Emp))  

(.<<.)     :: Data Integer -> Data Integer -> Data Integer
el .<<. er    = AppV shlIntVar (Ext el (Ext er Emp))  
 
complement :: Data Integer -> Data Integer
complement e  = AppV cmpIntVar (Ext e Emp)

testBit    :: Data Integer -> Data Integer -> Data Bool 
testBit el er =  ifThenElse ((el .&. (ConI 1 .<<. er)) == ConI 0) 
                 false  
                 true
 
oneBits :: Data Integer -> Data Integer
oneBits n = complement (complement (ConI 0) .<<. n)

lsbs :: Data Integer -> Data Integer -> Data Integer
lsbs k i = i .&. oneBits k

---------------------------------------------------------------------------------
-- Conversion Operators
---------------------------------------------------------------------------------
 
i2f :: Data Integer -> Data Float
i2f e = AppV i2fVar (Ext e Emp)

cis :: Data Float -> Data (Complex)
cis e = AppV cisVar (Ext e Emp)
 
vec2ary :: Vec (Data t) -> Data (TFA.Ary t)
vec2ary v = Ary (length v) (v !!) 

ary2vec :: HasSin TFG.Typ t => Data (TFA.Ary t) -> Vec (Data t)
ary2vec v = vec (Len v) (\i -> Ind v i)

---------------------------------------------------------------------------------
-- Vector Operators
---------------------------------------------------------------------------------

(...) :: Data Integer -> Data Integer -> Vec (Data Integer)
(...) m n = let l = ifThenElse (n < m) (ConI 0) (n - m + (ConI 1))
            in  vec l (\ i -> i + m)
  
permute :: (Data Integer -> Data Integer -> Data Integer)
           -> Vec t -> Vec t
permute f v = let l = length v 
              in  vec l (\ i -> v !! (f l i))
 
reverse :: Vec t -> Vec t
reverse = permute (\ l i -> l - (ConI 1) - i)

foldl :: HasSin TFG.Typ a => 
         (Data a -> Data b -> Data a) -> Data a -> Vec (Data b) -> Data a
foldl f acc v  = let l = length v
                 in  forLoop l acc (\ i a ->  f a (v !! i))
   

foldlVec :: HasSin TFG.Typ a => 
         (Vec (Data a) -> Data b -> Vec (Data a)) -> Vec (Data a) -> 
         Vec (Data b) -> Vec (Data a)
foldlVec f acc v  = let l = length v
                        acc' = vec2ary acc
                        f' vv d = vec2ary (f (ary2vec vv) d)
                    in  ary2vec (forLoop l acc' (\ i a ->  f' a (v !! i)))

map :: (a -> b) -> Vec a -> Vec b
map f v = let l = length v
          in vec l (\i -> f (v !! i))     

zipWith :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWith f v1 v2 = vec (min (length v1) (length v2))
                      (\ i -> f (v1 !! i) (v2 !! i))

sum :: Vec (Data Integer) -> Data Integer
sum = foldl (+) (ConI 0)

scalarProd :: Vec (Data Integer) -> Vec (Data Integer) -> Data Integer
scalarProd v1 v2 = sum (zipWith (*) v1 v2)

---------------------------------------------------------------------------------
-- Ary Operators
---------------------------------------------------------------------------------

(....) :: Data Integer -> Data Integer -> Data (Ary Integer)
(....) m n = let l = ifThenElse (n < m) (ConI 0) (n - m + (ConI 1))
             in  Ary l (\ i -> i + m)
  
permuteA :: HasSin TFG.Typ t => 
            (Data Integer -> Data Integer -> Data Integer)
           -> Data (Ary t) -> Data (Ary t)
permuteA f v = let l = lengthA v 
               in  Ary l (\ i -> v !!! (f l i))
 
reverseA :: HasSin TFG.Typ t => Data (Ary t) -> Data (Ary t)
reverseA = permuteA (\ l i -> l - (ConI 1) - i)

foldlA :: (HasSin TFG.Typ a , HasSin TFG.Typ b) => 
         (Data a -> Data b -> Data a) -> Data a -> Data (Ary b) -> Data a
foldlA f acc v  = let l = lengthA v
                  in  forLoop l acc (\ i a ->  f a (v !!! i))
   
mapA :: HasSin TFG.Typ a => 
        (Data a -> Data b) -> Data (Ary a) -> Data (Ary b)
mapA f v = let l = lengthA v
           in Ary l (\ i -> f (v !!! i))     

zipWithA :: (HasSin TFG.Typ a , HasSin TFG.Typ b) => 
            (Data a -> Data b -> Data c) -> Data (Ary a) -> Data (Ary b) -> 
           Data (Ary c)
zipWithA f v1 v2 = Ary (min (lengthA v1) (lengthA v2))
                      (\ i -> f (v1 !!! i) (v2 !!! i))

sumA :: Data (Ary Integer) -> Data Integer
sumA = foldlA (+) (ConI 0)

scalarProdA :: Data (Ary Integer) -> Data (Ary Integer) -> Data Integer
scalarProdA v1 v2 = sumA (zipWithA (*) v1 v2)

---------------------------------------------------------------------------------
-- Helper Operators
---------------------------------------------------------------------------------

fromList:: [Data a] -> Data a -> Vec (Data a)
fromList lst k =  vec
                  (litI (MP.fromIntegral (MP.length lst))) 
                  (\ i ->  MP.foldr 
                           (\ j acc -> ifThenElse 
                                       (i == 
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
                                       (i == 
                                        (litI (MP.fromIntegral j)))
                                       (lst MP.!! j)
                                       acc)
                           k         
                           (MP.enumFromTo 0 (MP.length lst MP.- 1)))