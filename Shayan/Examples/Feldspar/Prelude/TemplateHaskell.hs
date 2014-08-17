module Examples.Feldspar.Prelude.TemplateHaskell 
       (Data
       ,MP.Integer,litI
       ,MP.Float,litF                     
       ,MP.Bool(True,False)
       ,Tpl,{-((,)),-}fst,snd           
       ,Complex,cmx,real,imag
       ,Ary,ary,len,ind        
       ,{-ifThenElse,-}whl,forLoop,memorize
       ,not,and,or                  
       ,Equality(eql),notEql
       ,Ordering(lt),gt,lte,gte,min           
       ,Numeric(add,sub,mul,div,neg),ilog2,sqrt
       ,bitXor,bitAnd,bitOr,shfRgt,shfLft,complement,testBit,lsbs,oneBits
       ,i2f,cis
       ,frmTo,permute,reverse,foldl,map,zipWith,sum,scalarProd,fromList
       ) where

import MyPrelude (Integer,Array,Float,Bool(..))
import qualified MyPrelude as MP

import Language.Haskell.TH.Syntax (Lift(lift),Q,Exp(LitE),TExp
                                  ,Lit(IntegerL,RationalL))

import Examples.Feldspar.Prelude.Environment
import qualified VanillaPrelude as VP 

type Data t = Q (TExp t)

type Ary  t = Array Integer t

class    FO a                           where {}
instance FO Bool                        where {}
instance FO Integer                     where {}
instance FO Float                       where {}
instance (FO a , FO b) => FO (Tpl a b)  where {}
instance FO a => FO (Ary a)             where {}
instance FO Complex                     where {}

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

fst :: (FO a , FO b) => (a , b) -> a
fst = VP.fst

snd :: (FO a , FO b) => (a , b) -> b
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

ary :: FO a => Integer -> (Integer -> a) -> Ary a
ary = VP.ary

len :: FO a => Ary a -> Integer
len = VP.len

ind :: FO a => Ary a -> Integer -> a
ind = VP.ind

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

whl :: FO s => (s -> Bool) -> (s -> s) -> s -> s
whl = VP.whl

forLoop :: FO s => Data (Integer -> s -> (Integer -> s -> s) -> s)
forLoop = [|| \ l -> \ init -> \ step -> 
              snd (whl (\ t -> $$lt (fst t) l)
                       (\ t -> ( $$add (fst t) 1  
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

and :: Data (Bool -> Bool -> Bool)
and = [|| \ x -> \ y -> if x then y else False ||]

or :: Data (Bool -> Bool -> Bool)
or = [|| \ x -> \ y -> if x then True else y ||]

---------------------------------------------------------------------------------
-- Equality
---------------------------------------------------------------------------------
  
class Equality t where
  eql :: Data (t -> t -> Bool)
  
instance Equality Bool where
  eql = [|| eqlBolHsk ||] 

instance Equality Integer where
  eql = [|| eqlIntHsk ||]

instance Equality Float where
  eql = [|| eqlFltHsk ||]
         
notEql :: Equality t => Data (t -> t -> Bool)
notEql= [|| \ x -> \ y -> $$not ($$eql x y) ||]

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  lt :: Data (t -> t -> Bool)
   
instance Ordering Bool where
  lt = [|| \ x -> \ y -> ltdBolHsk x y ||] 

instance Ordering Integer where
  lt = [|| \ x -> \ y -> ltdIntHsk x y ||]   
  
instance Ordering Float where
  lt = [|| \ x -> \ y -> ltdFltHsk x y ||] 
    
gt :: (Equality t , Ordering t) => Data (t -> t -> Bool)
gt = [|| \ x -> \ y -> $$not ($$or ($$lt x y) ($$eql x y)) ||]

lte :: (Equality t , Ordering t) => Data (t -> t -> Bool)
lte = [|| \ x -> \ y -> $$or ($$lt x y) ($$eql x y) ||]

gte :: (Equality t , Ordering t) => Data (t -> t -> Bool)
gte = [|| \ x -> \ y -> $$not ($$lt x y) ||]

min :: Ordering t => Data(t -> t -> t) 
min = [|| \ x -> \ y -> if ($$lt x y) then x else y ||]

---------------------------------------------------------------------------------
-- Numeric
---------------------------------------------------------------------------------

class Numeric t where
  add :: Data (t -> t -> t) 
  sub :: Data (t -> t -> t)  
  mul :: Data (t -> t -> t)  
  div :: Data (t -> t -> t) 
  neg :: Data (t -> t)
 
instance Numeric Integer where
  add = [|| addIntHsk ||]
  sub = [|| subIntHsk ||]
  mul = [|| mulIntHsk ||]          
  div = [|| divIntHsk ||]
  neg = [|| \ i -> $$sub 0 i ||]  
    
instance Numeric Float where 
  add = [|| addFltHsk ||]
  sub = [|| subFltHsk ||]
  mul = [|| mulFltHsk ||]          
  div = [|| divFltHsk ||]
  neg = [|| \ f -> $$sub 0.0 f ||]

instance Numeric (Complex) where 
  add = [|| addCmxHsk ||]
  sub = [|| subCmxHsk ||]
  mul = [|| mulCmxHsk ||]          
  div = [|| divCmxHsk ||] 
  neg = [|| \ c -> $$sub (cmx 0.0 0.0) c ||]
  
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

bitAnd      :: Data (Integer -> Integer -> Integer) 
bitAnd         = [|| andIntHsk ||] 

bitOr      :: Data (Integer -> Integer -> Integer) 
bitOr         = [|| orIntHsk ||] 

bitXor        :: Data (Integer -> Integer -> Integer) 
bitXor        = [|| xorIntHsk ||] 

shfRgt     :: Data (Integer -> Integer -> Integer) 
shfRgt        = [|| shrIntHsk ||]  

shfLft     :: Data (Integer -> Integer -> Integer)
shfLft        = [|| shlIntHsk ||] 
 
complement :: Data (Integer -> Integer)
complement    = [|| cmpIntHsk ||] 

testBit    :: Data (Integer -> Integer -> Bool)
testBit       = [|| \ i -> \ j -> if $$eql ($$bitAnd i ($$shfLft 1 j)) 0 
                                  then False  
                                  else True ||] 
 
oneBits :: Data (Integer -> Integer)
oneBits       =  [|| \ n -> $$complement ($$shfLft ($$complement 0) n) ||]

lsbs :: Data (Integer -> Integer -> Integer)
lsbs          = [|| \ k -> \ i -> $$bitAnd i ($$oneBits k) ||]

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

frmTo :: (FrmInt t , Numeric t , FO t) => Data (Integer -> Integer -> Ary t)
frmTo = [|| \ m -> \ n -> ary 
                          (if ($$lt n m) 
                           then 0 
                           else ($$add ($$sub n m) 1))
                          (\ i -> $$add ($$frmInt i) ($$frmInt m)) ||]  
   
permute :: FO t => Data ((Integer -> Integer -> Integer) -> Ary t -> Ary t)
permute = [|| \ f -> \ v -> ary (len v) 
                                   (\ i -> ind v 
                                           (f (len v) i)) ||]
 
reverse :: FO t => Data (Ary t -> Ary t)
reverse = [|| $$permute (\ l i -> $$sub ($$sub l 1) i) ||]

foldl :: (FO a , FO b) => Data ((a -> b -> a) -> a -> Ary b -> a)
foldl = [|| \ f -> \ acc -> \ v -> 
            $$forLoop (len v) acc (\ i -> \ a -> f a (ind v i)) ||]
 
map :: (FO a , FO b) => Data ((a -> b) -> Ary a -> Ary b)
map = [|| \ f -> \ v -> ary (len v) (\ i -> f (ind v i)) ||]     

zipWith :: (FO a , FO b , FO c) => 
           Data ((a -> b -> c) -> Ary a -> Ary b -> Ary c)
zipWith = [|| \ f -> \ v1 -> \ v2 -> 
                ary ($$min (len v1) (len v2))
                    (\ i -> f (ind v1 i) (ind v2 i)) ||]

sum :: (FO t , Numeric t , FrmInt t) => Data (Ary t -> t)
sum = [|| $$foldl $$add ($$frmInt 0) ||]

scalarProd :: Data (Ary Integer -> Ary Integer -> Integer)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith $$mul v1 v2) ||]
 
fromList :: FO a => [Data a] -> Data a -> Data (Ary a)
fromList lst k =  let l = MP.fromInteger (MP.toInteger (MP.length lst))
                  in  [|| ary l
                          (\ i -> $$(MP.foldr 
                                     (\ j acc -> 
                                       let l' = (MP.fromInteger (MP.toInteger j))
                                       in  [|| if   $$eql i l'
                                               then $$(lst MP.!! j)
                                               else $$acc ||]) k 
                                      (MP.enumFromTo 0 (MP.length lst MP.- 1))
                                     )
                          ) 
                      ||]                      