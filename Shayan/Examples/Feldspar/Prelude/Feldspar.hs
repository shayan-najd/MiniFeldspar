module Examples.Feldspar.Prelude.Feldspar 
       (Data
       ,Integer,Integral(..),litI
       ,Float,Rational(..),litF
       ,Bool,true,false
       ,Tpl,tpl,fst,snd           
       ,Complex,complex,real,imag
       ,Vec,vec,length,(!!)           
       ,Ary,ary,len,ind                 
       ,ifThenElse,whl,forLoop,forLoopVec
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

import Feldspar 
  (Data,Float,Bool)

import qualified Feldspar        as F 
  (WordN
  ,value         
  ,true,false         
  ,Complex
  ,(==)
  ,(<)
  ,(+),(-),(*),(/),div
  ,(.|.),(.&.),xor,bitCount,testBit,complement,shiftRU,shiftLU
  ,i2n,cis
  ,condition)
 
import qualified Feldspar.Core.Frontend.Complex as C 
  (complex,realPart,imagPart)

import qualified Feldspar.Vector                as V 
  (Pull1,indexed1,length,(!!))

import qualified Feldspar.Core.Frontend.Array   as A
  (parallel,getLength,getIx)
  
import qualified Feldspar.Core.Frontend.Loop    as L
  (whileLoop) 

import qualified Feldspar.Core.Frontend as CF
  (sugar,desugar)

import Feldspar.Core.Types (Type)
 
---------------------------------------------------------------------------------
-- Integer
---------------------------------------------------------------------------------

type Integer = F.WordN
 
class Integral t where               
  fromInteger :: P.Integer -> t
 
instance Integral MP.Integer where
  fromInteger x = MP.fromIntegral x 

instance Integral (Data Integer) where
  fromInteger = F.value MP.. MP.fromIntegral

litI :: Integer -> Data Integer
litI = F.value

---------------------------------------------------------------------------------
-- Float
---------------------------------------------------------------------------------

class Rational t where
  fromRational :: P.Rational -> t
 
instance Rational MP.Float where
  fromRational x = MP.fromRational x

instance Rational (Data Float) where
  fromRational = F.value MP.. MP.fromRational

litF :: Float -> Data Float
litF = F.value

---------------------------------------------------------------------------------
-- Bool
---------------------------------------------------------------------------------
  
true :: Data Bool
true = F.true

false :: Data Bool
false = F.false

---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

type Tpl a b = (a , b)

tpl :: (Type a , Type b) => Data a -> Data b -> Data (a , b)
tpl x y = CF.desugar (x , y)

fst :: (Type a , Type b) => Data (a , b) -> Data a
fst = MP.fst MP.. (CF.sugar 
                       :: (Type a, Type b) => Data (a,b) -> (Data a, Data b))

snd :: (Type a , Type b) => Data (a , b) -> Data b
snd = MP.snd MP.. (CF.sugar 
                       :: (Type a, Type b) => Data (a,b) -> (Data a, Data b))

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

type Complex = F.Complex Float

complex :: Data Float -> Data Float -> Data Complex 
complex = C.complex

real :: Data Complex -> Data Float
real = C.realPart

imag :: Data Complex -> Data Float
imag = C.imagPart

---------------------------------------------------------------------------------
-- Vec
---------------------------------------------------------------------------------

type Vec t = V.Pull1 t

vec :: Data Integer -> (Data Integer -> Data a) -> Vec a 
vec = V.indexed1

length :: Vec a -> Data Integer
length = V.length

(!!) :: Vec a -> Data Integer -> Data a
(!!) = (V.!!)

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

type Ary t = [t]

ary :: Type t => Data Integer -> (Data Integer -> Data t) -> Data (Ary t) 
ary = A.parallel

len :: Type t => Data (Ary t) -> Data Integer
len = A.getLength

ind :: Type t => Data (Ary t) -> Data Integer -> Data t
ind = A.getIx

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

ifThenElse :: Type a => Data Bool -> Data a -> Data a -> Data a
ifThenElse = F.condition
 
whl :: Type t => (Data t -> Data Bool) -> (Data t -> Data t) -> Data t -> Data t
whl ec eb ei = L.whileLoop ei ec eb

 
forLoop :: Type s => Data Integer -> Data s -> 
           (Data Integer -> Data s -> Data s ) -> Data s
forLoop l init step = snd (whl (\ t -> (fst t) < l)
                               (\ t -> tpl 
                                       ((fst t) + (litI 1)) 
                                       (step (fst t) (snd t)))
                               (tpl (litI 0) init)) 
                      
forLoopVec :: Type s => Data Integer -> Vec s -> 
              (Data Integer -> Vec s -> Vec s) -> Vec s
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
  (==) = (F.==)

instance Equality Integer where
  (==) = (F.==)

instance Equality Float where
  (==) = (F.==)
         
(/=) :: Equality t => Data t -> Data t -> Data Bool 
x /= y = not (x == y) 

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  (<) :: Data t -> Data t -> Data Bool
   
instance Ordering Bool where
  (<) = (F.<)

instance Ordering Integer where
  (<) = (F.<)

instance Ordering Float where
  (<) = (F.<)
 
(>) :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
x > y = not ((x < y) || (x == y)) 

(>=) :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
x >= y = not (x < y)

(<=) :: (Equality t , Ordering t) => Data t -> Data t -> Data Bool
x <= y = (x < y) || (x == y)

min :: (Type t , Ordering t) => Data t -> Data t -> Data t
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
  (+)     = (F.+)
  (-)     = (F.-)
  (*)     = (F.*)
  (/)     = F.div
  negate  = (0 -) 

instance Numeric Float where 
  (+)    = (F.+)
  (-)    = (F.-)
  (*)    = (F.*)
  (/)    = (F./)
  negate = (0 -)

instance Numeric (Complex) where 
  (+)    = (F.+)
  (-)    = (F.-)
  (*)    = (F.*)
  (/)    = (F./)
  negate = (0 -)
 
ilog2 :: Data Integer -> Data Integer
ilog2 xx = 31 - nlz xx
 where 
   nlz :: Data Integer -> Data Integer
   nlz x = bitCount (complement (part x))
   
   part :: Data Integer -> Data Integer
   part x = 
     ((((x .|. (x .>>. 1)) .|. ((x .|. (x .>>. 1)) .>>. 2)) .|. 
       (((x .|. (x .>>. 1)) .|. ((x .|. (x .>>. 1)) .>>. 2)) .>>. 4)) .|. 
      ((((x .|. (x .>>. 1)) .|. ((x .|. (x .>>. 1)) .>>. 2)) .|. 
        (((x .|. (x .>>. 1)) .|. ((x .|. (x .>>. 1)) .>>. 2)) .>>. 4)) .>>. 8)) 
     .|. 
     (((((x .|. (x .>>. 1)) .|. ((x .|. (x .>>. 1)) .>>. 2)) .|. 
        (((x .|. (x .>>. 1)) .|. ((x .|. (x .>>. 1)) .>>. 2)) .>>. 4)) .|. 
       ((((x .|. (x .>>. 1)) .|. ((x .|. (x .>>. 1)) .>>. 2)) .|. 
         (((x .|. (x .>>. 1)) .|. 
           ((x .|. (x .>>. 1)) .>>. 2)) .>>. 4)) .>>. 8)) .>>. 16)

-- pi :: Data Float
-- pi = lit (MP.pi)

---------------------------------------------------------------------------------
-- Bitwise Operators
---------------------------------------------------------------------------------

(.&.)      :: Data Integer -> Data Integer -> Data Integer 
(.&.)      = (F..&.)

(.|.)      :: Data Integer -> Data Integer -> Data Integer
(.|.)      = (F..|.)

xor        :: Data Integer -> Data Integer -> Data Integer
xor        = F.xor

(.>>.)     :: Data Integer -> Data Integer -> Data Integer
(.>>.)     = F.shiftRU

(.<<.)     :: Data Integer -> Data Integer -> Data Integer
(.<<.)     = F.shiftLU

bitCount   :: Data Integer -> Data Integer
bitCount   = F.bitCount

testBit    :: Data Integer -> Data Integer -> Data Bool 
testBit    = F.testBit

complement :: Data Integer -> Data Integer
complement = F.complement
 
oneBits :: Data Integer -> Data Integer
oneBits n = complement (complement 0 .<<. n)

lsbs :: Data Integer -> Data Integer -> Data Integer
lsbs k i = i .&. oneBits k

---------------------------------------------------------------------------------
-- Conversion Operators
---------------------------------------------------------------------------------
 
i2f :: Data Integer -> Data Float
i2f = F.i2n

cis :: Data Float -> Data (Complex)
cis = F.cis

vec2ary :: Type t => Vec t -> Data (Ary t)
vec2ary v = ary (length v) (v !!) 

ary2vec :: Type t => Data (Ary t) -> Vec t
ary2vec v = vec (len v) (\i -> ind v i)
 
---------------------------------------------------------------------------------
-- Vector Operators
---------------------------------------------------------------------------------

(...) :: Data Integer -> Data Integer -> Vec Integer
(...) m n = let l = ifThenElse (n < m) 0 (n - m + 1)
            in  vec l (\ i -> i + m)
  
permute :: (Data Integer -> Data Integer -> Data Integer)
           -> Vec t -> Vec t
permute f v = let l = length v 
              in  vec l (\ i -> v !! (f l i))
 
reverse :: Vec t -> Vec t
reverse = permute (\ l i -> l - 1 - i)

foldl :: Type a => (Data a -> Data b -> Data a) -> Data a -> Vec b -> Data a
foldl f acc v  = let l = length v
                 in  forLoop l acc (\ i a ->  f a (v !! i))

foldlVec :: Type a => 
            (Vec a -> Data b -> Vec a) -> Vec a -> Vec b -> Vec a
foldlVec f acc v  = let l = length v
                        acc' = vec2ary acc
                        f' vv d = vec2ary (f (ary2vec vv) d)
                    in  ary2vec (forLoop l acc' (\ i a ->  f' a (v !! i)))

map :: (Data a -> Data b) -> Vec a -> Vec b
map f v = let l = length v
          in vec l (\i -> f (v !! i))     

zipWith :: (Data a -> Data b -> Data c) -> Vec a -> Vec b -> Vec c
zipWith f v1 v2 = vec (min (length v1) (length v2))
                      (\ i -> f (v1 !! i) (v2 !! i))

sum :: Vec Integer -> Data Integer
sum = foldl (+) 0

scalarProd :: Vec Integer -> Vec Integer -> Data Integer
scalarProd v1 v2 = sum (zipWith (*) v1 v2)

---------------------------------------------------------------------------------
-- Ary Operators
---------------------------------------------------------------------------------

(....) :: Data Integer -> Data Integer -> Data (Ary Integer)
(....) m n = let l = ifThenElse (n < m) (litI 0) (n - m + (litI 1))
             in  ary l (\ i -> i + m)
  
permuteA :: Type t => 
            (Data Integer -> Data Integer -> Data Integer)
           -> Data (Ary t) -> Data (Ary t)
permuteA f v = let l = len v 
               in  ary l (\ i -> ind v (f l i))
 
reverseA :: Type t => Data (Ary t) -> Data (Ary t)
reverseA = permuteA (\ l i -> l - (litI 1) - i)

foldlA :: (Type a , Type b) => 
         (Data a -> Data b -> Data a) -> Data a -> Data (Ary b) -> Data a
foldlA f acc v  = let l = len v
                  in  forLoop l acc (\ i a ->  f a (ind v i))
   
mapA :: (Type a , Type b) => 
        (Data a -> Data b) -> Data (Ary a) -> Data (Ary b)
mapA f v = let l = len v
           in ary l (\ i -> f (ind v i))     

zipWithA :: (Type a , Type b , Type c) => 
            (Data a -> Data b -> Data c) -> Data (Ary a) -> Data (Ary b) -> 
           Data (Ary c)
zipWithA f v1 v2 = ary (min (len v1) (len v2))
                      (\ i -> f (ind v1 i) (ind v2 i))

sumA :: Data (Ary Integer) -> Data Integer
sumA = foldlA (+) (litI 0)

scalarProdA :: Data (Ary Integer) -> Data (Ary Integer) -> Data Integer
scalarProdA v1 v2 = sumA (zipWithA (*) v1 v2)

---------------------------------------------------------------------------------
-- Helper Operators
---------------------------------------------------------------------------------

fromList:: Type a => [Data a] -> Data a -> Vec a
fromList lst k =  vec
                  (F.value (MP.fromInteger (MP.toInteger (MP.length lst)))) 
                  (\ i ->  MP.foldr 
                           (\ j acc -> ifThenElse 
                                       (i == 
                                        (F.value (MP.fromInteger 
                                                  (MP.toInteger j))))
                                       (lst MP.!! j)
                                       acc)
                           k         
                           (MP.enumFromTo 0 (MP.length lst MP.- 1)))

fromListA :: Type a => [Data a] -> Data a -> Data (Ary a)
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