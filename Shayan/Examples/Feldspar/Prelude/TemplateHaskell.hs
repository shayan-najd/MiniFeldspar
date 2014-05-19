module Examples.Feldspar.Prelude.TemplateHaskell 
       (Data
       ,MP.Integer,litI
       ,MP.Float,litF                     
       ,MP.Bool(True,False)
       ,Tpl,{-((,)),-}fst,snd           
       ,Complex,cmx,real,imag
       ,Ary,ary,len,ind        
       ,{-ifThenElse,-}whl,forLoop,memorize
       ,not,(&&),(||)                  
       ,Equality((==)),(/=)
       ,Ordering((<)),(>),(>=),(<=),min           
       ,Numeric((+),(-),(*),(/),negate),ilog2,sqrt
       ,xor,(.&.),(.|.),(.>>.),(.<<.),complement,testBit,lsbs,oneBits
       ,i2f,cis
       ,(...),permute,reverse,foldl,map,zipWith,sum,scalarProd,fromList
       ) where

import Prelude ()
import MyPrelude (Integer,Array,Float,Bool(..))
import qualified MyPrelude as MP

import Language.Haskell.TH.Syntax
 
import Examples.Feldspar.Prelude.Environment
import qualified VanillaPrelude as VP 

type Data t = Q (TExp t)

type Ary t = Array Integer t

class FO a                              where {}
instance FO Bool                        where {}
instance FO Integer                     where {}
instance FO Float                       where {}
instance FO Complex                     where {}
instance (FO a , FO b) => FO (Tpl a b)  where {}
instance FO a => FO (Ary a)             where {}

---------------------------------------------------------------------------------
-- Integer
---------------------------------------------------------------------------------
 
instance Lift Integer where
    lift i = MP.return (LitE (IntegerL (MP.toInteger i)))
 
litI :: Integer -> Data Integer
litI i = [|| i ||]

class FrmInt t where
    frmInt :: Data (Integer -> t)

instance FrmInt Integer where
    frmInt =  [|| \ i -> i ||]

---------------------------------------------------------------------------------
-- Float
---------------------------------------------------------------------------------
 
instance Lift Float where
  lift f = MP.return (LitE (RationalL (MP.toRational f)))

litF :: Float -> Data Float
litF f = [|| f ||]

instance FrmInt Float where
    frmInt = i2f

---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

type Tpl a b = (a , b)

fst :: (a , b) -> a
fst = VP.fst

snd :: (a , b) -> b
snd = VP.snd

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

type Complex = MP.Complex Float
 
cmx :: Float -> Float -> Complex
cmx = VP.cmx

real :: Data (Complex -> Float)
real = [|| realPartHsk ||]

imag :: Data (Complex -> Float)
imag = [|| imagPartHsk ||]

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

ary :: Integer -> (Integer -> a) -> Ary a
ary = VP.ary

len :: Ary a -> Integer
len = VP.len

ind :: Ary a -> Integer -> a
ind = VP.ind

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

whl :: FO s => (s -> Bool) -> (s -> s) -> s -> s
whl = VP.whl

forLoop :: FO s => Data (Integer -> s -> (Integer -> s -> s) -> s)
forLoop = [|| \ l -> \ init -> \ step -> 
              snd (whl (\ t -> $$((<)) (fst t) l)
                       (\ t -> ( $$((+)) (fst t) 1  
                               , step (fst t) (snd t)))
                   (0 , init)) 
          ||]

memorize :: Data (Ary Float -> Ary Float)
memorize = [|| memHsk ||]
          
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
  negate :: Data (t -> t)
 
instance Numeric Integer where
  (+) = [|| addIntHsk ||]
  (-) = [|| subIntHsk ||]
  (*) = [|| mulIntHsk ||]          
  (/) = [|| divIntHsk ||]
  negate = [|| \ i -> $$((-)) 0 i ||]  
    
instance Numeric Float where 
  (+) = [|| addFltHsk ||]
  (-) = [|| subFltHsk ||]
  (*) = [|| mulFltHsk ||]          
  (/) = [|| divFltHsk ||]
  negate = [|| \ f -> $$((-)) 0.0 f ||]

instance Numeric (Complex) where 
  (+) = [|| addCmxHsk ||]
  (-) = [|| subCmxHsk ||]
  (*) = [|| mulCmxHsk ||]          
  (/) = [|| divCmxHsk ||] 
  negate = [|| \ c -> $$((-)) (cmx 0.0 0.0) c ||]
  
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

sqrt :: Data (Float -> Float)
sqrt = [|| sqrtFltHsk ||]

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
 
complement :: Data (Integer -> Integer)
complement    = [|| cmpIntHsk ||] 

testBit    :: Data (Integer -> Integer -> Bool)
testBit       = [|| \ i -> \ j -> if $$((==)) ($$((.&.)) i ($$((.<<.)) 1 j)) 0 
                                  then False  
                                  else True ||] 
 
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

---------------------------------------------------------------------------------
-- Array Operators
---------------------------------------------------------------------------------

(...) :: (FrmInt t , Numeric t) => Data (Integer -> Integer -> Ary t)
(...) = [|| \ m -> \ n -> ary 
                          (if ($$((<)) n m) 
                           then 0 
                           else ($$((+)) ($$((-)) n m) 1))
                          (\ i -> $$((+)) ($$frmInt i) ($$frmInt m)) ||]  
   
permute :: Data ((Integer -> Integer -> Integer) -> Ary t -> Ary t)
permute = [|| \ f -> \ v -> ary (len v) 
                                   (\ i -> ind v 
                                           (f (len v) i)) ||]
 
reverse :: Data (Ary t -> Ary t)
reverse = [|| $$permute (\ l i -> $$((-)) ($$((-)) l 1) i) ||]

foldl :: FO a => Data ((a -> b -> a) -> a -> Ary b -> a)
foldl = [|| \ f -> \ acc -> \ v -> 
            $$forLoop (len v) acc (\ i -> \ a ->  f a (ind v i)) ||]
 
map :: Data ((a -> b) -> Ary a -> Ary b)
map = [|| \ f -> \ v -> ary (len v) (\ i -> f (ind v i)) ||]     

zipWith :: Data ((a -> b -> c) -> Ary a -> Ary b -> Ary c)
zipWith = [|| \ f -> \ v1 -> \ v2 -> 
                ary ($$min (len v1) (len v2))
                    (\ i -> f (ind v1 i) (ind v2 i)) ||]

sum :: (FO t , Numeric t , FrmInt t) => Data (Ary t -> t)
sum = [|| $$foldl $$((+)) ($$frmInt 0) ||]

scalarProd :: Data (Ary Integer -> Ary Integer -> Integer)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith $$((*)) v1 v2) ||]
 
fromList :: [Data a] -> Data a -> Data (Ary a)
fromList lst k =  let l = MP.fromInteger (MP.toInteger (MP.length lst))
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