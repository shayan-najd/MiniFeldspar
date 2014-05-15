module Examples.Feldspar.Prelude.TemplateHaskell 
       (Data
       ,MP.Integer,litI
       ,MP.Float,litF                     
       ,MP.Bool(True,False)
       ,Tpl,tpl,VP.fst,VP.snd           
       ,Complex,VP.cmx,real,imag
       ,Vec,vec,length,(!!)           
       ,Ary,VP.ary,lengthA,(!!!)        
       ,{-ifThenElse,-}forLoop,forLoopVec,VP.whl
       ,not,(&&),(||)                  
       ,Equality((==)),(/=)
       ,Ordering((<)),(>),(>=),(<=),min           
       ,Numeric((+),(-),(*),(/)),ilog2
       ,xor,(.&.),(.|.),(.>>.),(.<<.),bitCount,testBit,complement,lsbs,oneBits
       ,i2f,cis,ary2vec,vec2ary                                  
       ,(...),permute,reverse,foldl,map,zipWith,sum,scalarProd,fromList
       ,(....),permuteA,reverseA,foldlA,mapA,zipWithA,sumA,scalarProdA,fromListA

) where

import Prelude ()
import MyPrelude (Integer,Array,Float,Bool(..))
import qualified MyPrelude as MP

import Language.Haskell.TH.Syntax
--import qualified Type.Feldspar.ADT             as TFA
--import qualified Type.Feldspar.GADT            as TFG

-- import Singleton
-- import Environment.Typed hiding (map)
import Examples.Feldspar.Prelude.Environment
import VanillaPrelude as VP

{-
import Feldspar 
  (Data,Float,Bool,condition,forLoop)

import qualified Feldspar        as F 
  (WordN
  ,value         
  ,true,false         
  ,Complex
  ,(==)
  ,(<)
  ,(+),(-),(*),(/),div
  ,(.|.),(.&.),xor,bitCount,testBit,complement,shiftRU,shiftLU
  ,i2n,cis)
  
import qualified Feldspar.Core.Frontend.Complex as C 
  (complex,realPart,imagPart)
  
import qualified Feldspar.Vector as V 
  (Pull1,indexed1,length,(!!))

import Feldspar.Core.Types (Type)
-}

type Data t = Q (TExp t)
type Ary t = Array Integer t
 
instance Lift Integer where
  lift i = MP.return (LitE (IntegerL (MP.toInteger i)))
 
instance Lift Float where
  lift f = MP.return (LitE (RationalL (MP.toRational f)))


litI :: Integer -> Data Integer
litI i = [|| i ||]

litF :: Float -> Data Float
litF f = [|| f ||]

---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

type Tpl a b = (a , b)

tpl :: Data (a -> b -> Tpl a b)
tpl = [|| \ x -> \ y -> (x , y) ||]

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

type Complex = MP.Complex Float
 
real :: Data (Complex -> Float)
real = [|| realPartHsk ||]

imag :: Data (Complex -> Float)
imag = [|| imagPartHsk ||]

---------------------------------------------------------------------------------
-- Vec
---------------------------------------------------------------------------------

type Vec t = (Integer , Integer -> t)
 
vec :: Data (Integer -> (Integer -> t) -> Vec t)
vec = [|| \ l -> \ f ->  (l , f) ||]

length :: Data (Vec t -> Integer)
length = [|| \ v -> fst v ||]

(!!) :: Data (Vec t -> Integer -> t)
(!!) = [|| \ v -> snd v ||] 

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

lengthA :: Data (Ary a -> Integer)
lengthA = [|| len ||]

(!!!) :: Data (Ary a -> Integer -> a)
(!!!) = [|| ind ||]

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

forLoop :: Data (Integer -> s -> (Integer -> s -> s) -> s)
forLoop = [|| \ l -> \ init -> \ step -> 
              snd (whl (\ t -> $$((<)) (fst t) l)
                       (\ t -> ( $$((+)) (fst t) 1  
                               , step (fst t) (snd t)))
                   (0 , init)) 
          ||]
 
forLoopVec :: Data (Integer -> Vec s -> 
                    (Integer -> Vec s -> Vec s) -> Vec s)
forLoopVec = [|| \ l -> \ init -> \ step ->  
                $$ary2vec ($$forLoop l ($$vec2ary init) 
                           (\ i -> \ a -> $$vec2ary (step i ($$ary2vec a)))) 
             ||]

---------------------------------------------------------------------------------
-- Boolean Operators
---------------------------------------------------------------------------------

not :: Data (Bool -> Bool)
not = [||  \ x -> if x then False else True ||] 

(&&) :: Data (Bool -> Bool -> Bool)
(&&) = [|| \ x -> \ y -> if x then y else False ||]

(||) :: Data (Bool -> Bool -> Bool)
(||) = [|| \ x -> \ y -> if x then True else y ||]

---------------------------------------------------------------------------------
-- Equality
---------------------------------------------------------------------------------
  
class Equality t where
  (==) :: Data (t -> t -> Bool)
  
instance Equality Bool where
  (==) = [|| eqlBolHsk ||] 

instance Equality Integer where
  (==) = [|| eqlIntHsk ||]

instance Equality Float where
  (==) = [|| eqlFltHsk ||]
         
(/=) :: Equality t => Data (t -> t -> Bool)
(/=)= [|| \ x -> \ y -> $$not (($$((==))) x y) ||]


---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  (<) :: Data (t -> t -> Bool)
   
instance Ordering Bool where
  (<) = [|| \ x -> \ y -> ltdBolHsk x y ||] 

instance Ordering Integer where
  (<) = [|| \ x -> \ y -> ltdIntHsk x y ||]   
  
instance Ordering Float where
  (<) = [|| \ x -> \ y -> ltdFltHsk x y ||] 
    
(>) :: (Equality t , Ordering t) => Data (t -> t -> Bool)
(>) = [|| \ x -> \ y -> $$not ($$((||)) ($$((<)) x y) ($$((==)) x y)) ||]

(>=) :: (Equality t , Ordering t) => Data (t -> t -> Bool)
(>=) = [|| \ x -> \ y -> $$not ($$((<)) x y) ||]

(<=) :: (Equality t , Ordering t) => Data (t -> t -> Bool)
(<=) = [|| \ x -> \ y -> $$((||)) ($$((<)) x y) ($$((==)) x y) ||]

min :: Ordering t => Data(t -> t -> t) 
min = [|| \ x -> \ y -> if ($$((<)) x y) then x else y ||]

---------------------------------------------------------------------------------
-- Numeric
---------------------------------------------------------------------------------

class Numeric t where
  (+) :: Data (t -> t -> t) 
  (-) :: Data (t -> t -> t)  
  (*) :: Data (t -> t -> t)  
  (/) :: Data (t -> t -> t) 
 
instance Numeric Integer where
  (+) = [|| addIntHsk ||]
  (-) = [|| subIntHsk ||]
  (*) = [|| mulIntHsk ||]          
  (/) = [|| divIntHsk ||]
    
instance Numeric Float where 
  (+) = [|| addFltHsk ||]
  (-) = [|| subFltHsk ||]
  (*) = [|| mulFltHsk ||]          
  (/) = [|| divFltHsk ||]

instance Numeric (Complex) where 
  (+) = [|| addCmxHsk ||]
  (-) = [|| subCmxHsk ||]
  (*) = [|| mulCmxHsk ||]          
  (/) = [|| divCmxHsk ||] 
  
ilog2 :: Data (Integer -> Integer)
ilog2 = [|| ilog2Hsk ||] 
  {-
  [|| \ xx -> ($$((-))) 31  ($$nlz xx) ||]
 where
   nlz :: Data (Integer -> Integer)
   nlz = [|| \ x -> $$bitCount ($$complement 
                                $$(MP.foldl go [|| x ||] [1,2,4,8,16])) ||]
     where
       go :: Data Integer -> Integer -> Data Integer
       go b s = [|| $$((.|.)) $$b  ($$((.>>.)) $$b s) ||]
   -}
---------------------------------------------------------------------------------
-- Bitwise Operators
---------------------------------------------------------------------------------

(.&.)      :: Data (Integer -> Integer -> Integer) 
(.&.)         = [|| andIntHsk ||] 

(.|.)      :: Data (Integer -> Integer -> Integer) 
(.|.)         = [|| orIntHsk ||] 

xor        :: Data (Integer -> Integer -> Integer) 
xor           = [|| xorIntHsk ||] 

(.>>.)     :: Data (Integer -> Integer -> Integer) 
(.>>.)        = [|| shrIntHsk ||]  

(.<<.)     :: Data (Integer -> Integer -> Integer)
(.<<.)        = [|| shlIntHsk ||] 

bitCount   :: Data (Integer -> Integer)
bitCount      = [|| btcIntHsk ||] 

testBit    :: Data (Integer -> Integer -> Bool)
testBit       = [|| tsbIntHsk ||] 

complement :: Data (Integer -> Integer)
complement    = [|| cmpIntHsk ||] 
 
oneBits :: Data (Integer -> Integer)
oneBits       =  [|| \ n -> $$complement (($$((.<<.)))($$complement 0) n) ||]

lsbs :: Data (Integer -> Integer -> Integer)
lsbs          = [|| \ k -> \ i -> ($$((.&.))) i  ($$oneBits k) ||]

---------------------------------------------------------------------------------
-- Conversion Operators
---------------------------------------------------------------------------------
 
i2f :: Data (Integer -> Float)
i2f = [|| i2fHsk ||] 

cis :: Data (Float -> Complex)
cis = [|| cisHsk ||] 
 
vec2ary :: Data (Vec t -> Ary t)
vec2ary = [|| \ v -> ary ($$length v) (\ i -> $$((!!)) v i) ||] 

ary2vec :: Data (Ary t -> Vec t)
ary2vec = [|| \ v -> $$vec (len v) (\i -> ind v i) ||]

---------------------------------------------------------------------------------
-- Vector Operators
---------------------------------------------------------------------------------

(...) :: Data (Integer -> Integer -> Vec Integer)
(...) = [|| \ m -> \ n -> $$vec 
                          (if ($$((<)) n m) 
                           then 0 
                           else ($$((+)) ($$((-)) n m) 1))
                           (\ i -> $$((+)) i m) ||]  
   
permute :: Data ((Integer -> Integer -> Integer) -> Vec t -> Vec t)
permute = [|| \ f -> \ v -> $$vec ($$length v) (\ i -> $$((!!)) v 
                                                     (f ($$length v) i)) ||]
 
reverse :: Data (Vec t -> Vec t)
reverse = [|| $$permute (\ l i -> $$((-)) ($$((-)) l 1) i) ||]

foldl ::  Data ((a -> b -> a) -> a -> Vec b -> a)
foldl = [|| \ f -> \ acc -> \ v -> 
           $$forLoop ($$length v) acc (\ i -> \ a ->  f a ($$((!!)) v i)) ||]
{-   
foldlVec :: Data ((Vec Complex -> Integer -> Vec Complex) -> 
                  Vec Complex -> Vec Integer -> Vec Complex)
foldlVec = [|| \ f -> \ acc -> \ v ->
               $$ary2vec ($$foldl (\ a i -> $$vec2ary (f ($$ary2vec a) i)) 
                                  ($$vec2ary acc) v)||]
-}   

map :: Data ((a -> b) -> Vec a -> Vec b)
map = [|| \ f -> \ v -> $$vec ($$length v) (\ i -> f ($$((!!)) v i)) ||]     

zipWith :: Data ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipWith = [|| \ f -> \ v1 -> \ v2 -> 
              $$vec ($$min ($$length v1) ($$length v2))
                    (\ i -> f ($$((!!)) v1 i) ($$((!!)) v2 i)) ||]

sum :: Data (Vec Integer -> Integer)
sum = [|| $$foldl $$((+)) 0 ||]


scalarProd :: Data (Vec Integer -> Vec Integer -> Integer)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith $$((*)) v1 v2) ||]

fromList:: [Data a] -> Data a -> Data (Vec a)
fromList lst k =  let l = MP.fromInteger (MP.toInteger (MP.length lst))
                  in  [|| $$vec l
                          (\ i -> $$(MP.foldr 
                                     (\ j acc -> 
                                       let l' = (MP.fromInteger (MP.toInteger j))
                                       in  [|| if   $$((==)) i l'
                                               then $$(lst MP.!! j)
                                               else $$acc ||]) k 
                                      (MP.enumFromTo 0 (MP.length lst MP.- 1))
                                     )
                          ) 
                      ||]
                      
---------------------------------------------------------------------------------
-- Array Operators
---------------------------------------------------------------------------------

(....) :: Data (Integer -> Integer -> Ary Integer)
(....) = [|| \ m -> \ n -> ary 
                          (if ($$((<)) n m) 
                           then 0 
                           else ($$((+)) ($$((-)) n m) 1))
                          (\ i -> $$((+)) i m) ||]  
   
permuteA :: Data ((Integer -> Integer -> Integer) -> Ary t -> Ary t)
permuteA = [|| \ f -> \ v -> ary ($$lengthA v) 
                                   (\ i -> $$((!!!)) v 
                                           (f ($$lengthA v) i)) ||]
 
reverseA :: Data (Ary t -> Ary t)
reverseA = [|| $$permuteA (\ l i -> $$((-)) ($$((-)) l 1) i) ||]

foldlA ::  Data ((a -> b -> a) -> a -> Ary b -> a)
foldlA = [|| \ f -> \ acc -> \ v -> 
            $$forLoop ($$lengthA v) acc (\ i -> \ a ->  f a ($$((!!!)) v i)) ||]
{-   
foldlAry :: Data ((Ary Complex -> Integer -> Ary Complex) -> 
                  Ary Complex -> Ary Integer -> Ary Complex)
foldlAry = [|| \ f -> \ acc -> \ v ->
               $$ary2vec ($$foldl (\ a i -> $$vec2ary (f ($$ary2vec a) i)) 
                                  ($$vec2ary acc) v)||]
-}   

mapA :: Data ((a -> b) -> Ary a -> Ary b)
mapA = [|| \ f -> \ v -> ary ($$lengthA v) (\ i -> f ($$((!!!)) v i)) ||]     

zipWithA :: Data ((a -> b -> c) -> Ary a -> Ary b -> Ary c)
zipWithA = [|| \ f -> \ v1 -> \ v2 -> 
                ary ($$min ($$lengthA v1) ($$lengthA v2))
                    (\ i -> f ($$((!!!)) v1 i) ($$((!!!)) v2 i)) ||]

sumA :: Data (Ary Integer -> Integer)
sumA = [|| $$foldlA $$((+)) 0 ||]


scalarProdA :: Data (Ary Integer -> Ary Integer -> Integer)
scalarProdA  = [|| \ v1 -> \ v2 -> $$sumA ($$zipWithA $$((*)) v1 v2) ||]

fromListA :: [Data a] -> Data a -> Data (Ary a)
fromListA lst k =  let l = MP.fromInteger (MP.toInteger (MP.length lst))
                  in  [|| ary l
                          (\ i -> $$(MP.foldr 
                                     (\ j acc -> 
                                       let l' = (MP.fromInteger (MP.toInteger j))
                                       in  [|| if   $$((==)) i l'
                                               then $$(lst MP.!! j)
                                               else $$acc ||]) k 
                                      (MP.enumFromTo 0 (MP.length lst MP.- 1))
                                     )
                          ) 
                      ||]