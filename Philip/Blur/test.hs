-- functions special to the Feldspar compiler

data Ary a = Ary { len :: Int, ind :: Int -> a }

arr :: Int -> (Int -> x) -> Array Int x
arr m f  =  array (0, m-1) [ (i, f i) | i <- [0..m-1] ]

while :: FO a => Exp ((x -> Bool) -> (x -> x) -> (x -> x))
while p f x  =  if p x then while p f (f x) else x

-- standard library functions for Feldspar

upto :: Exp (Int -> Ary Float)
upto =  [|| \ m -> Ary m (\i -> toFloat i) ||]

sum :: Num x => (Ary x -> x)
sum =  [|| \ a -> while (\ (i,s) -> i < len a) (\ (i,s) -> (i+1, s + ind a i)) (0,0)

append :: Exp (Ary x -> Ary x -> Ary x)
append =  [|| \ a b -> Ary (len a + len b) (\i -> if i < m then ind a i else ind b (i-m)) ||]

unit :: Exp (x -> Ary x)
unit =  [|| \ x -> Ary 1 (\i -> x) ||]

zipWith :: Exp ((x -> y -> z) -> Ary x -> Ary y -> Ary z)
zipWith =  [|| \ f a b -> if len a == len b then Ary (len a) (\i -> f (ind a i) (ind b i)) else error ||]

memorize :: Exp (Ary a -> Ary a)
memorize =  [|| \ a -> let am = arr (len a) (ind a) in Ary (len a) (\i -> am ! i) ||]

-- functions specific to this example

geometric :: Exp (Float -> Float -> Float)
geometric =  [|| \ x y -> sqrt (sqr x) (sqr y) ||]

blur :: Exp (Ary Float -> Ary Float)
blur =  [|| \ a -> $zipWith $geometric ($append ($unit 0) a) ($append a ($unit 0)) ||]

size :: Exp Int
size =  100000

test :: Exp Float
test =  [|| $sum ($blur ($upto $size)) ||]

test2 :: Exp Float
test2 =  [|| $sum ($blur ($blur ($upto $size))) ||]

test2m :: Exp Float
test2m =  [|| $sum ($blur ($memorize ($blur ($upto $size)))) ||]
