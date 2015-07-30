module SimpleLens where
import Control.Applicative
import Data.List
-- based on http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers_28.html
-- cabal instll HUnit
-- rm -f *.o && rm -f Lens && rm -f *.tix && ghc -fhpc Lens.hs && ./Lens && hpc markup Lens
-- view coverage in Main.hs.html

-- Point data type
data Point  = Point  { x :: Double, y :: Double }
    deriving (Show, Eq)

getX :: Point -> Double
getX = x

setX :: Double -> Point -> Point
setX x'' p = p { x = x'' }

getY :: Point -> Double
getY = y

setY :: Double -> Point -> Point
setY y'' p = p { y = y'' }

-- Circle datatype
data Circle = Circle { center :: Point, radius :: Double }
    deriving (Show, Eq)

getCenter :: Circle -> Point
getCenter = center

setCenter :: Point -> Circle -> Circle
setCenter center'' c = c {center = center''}

getR :: Circle -> Double
getR = radius

setR :: Double -> Circle -> Circle
setR r' c = c {radius = r'}

data Surface = Surface { points :: [Point] }
    deriving (Show, Eq)

getPoints :: Surface -> [Point]
getPoints = points

setPoints :: [Point] -> Surface -> Surface
setPoints ps s = s { points = ps }

-- Lens type
type Lens a b = ( a -> b , b -> a -> a )

getL :: Lens a b -> a -> b
getL (g, _) = g

setL :: Lens a b -> b -> a -> a
setL (_, s) = s

-- Apply a function to the field
-- I consider modL more fundamental than setL
modL :: Lens a b -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a

-- Lens combination operator
(<.>):: Lens a b -> Lens b c -> Lens a c
(getA, setA) <.> (getB, setB) = (getB . getA, \z x -> setA (setB z (getA x)) x)

traversed :: Lens a [b] -> Lens b c -> Lens a [c]
traversed (getA, setA) (getB, setB) = (\a -> map getB $ getA a, \ns a -> setA (map (uncurry setB) (zip ns (getA a))) a)

filtred :: (b -> Bool) -> Lens a [b] -> Lens b c -> Lens a [c]
filtred pred (getA, setA) (getB, setB) = (\a -> map getB $ filter pred $ getA a, \vs a -> setA (map (uncurry setB) (zip (get_val pred (getA a) (map getB $ getA a) vs []) (getA a))) a)

get_val :: (a -> Bool) -> [a] -> [b] -> [b] -> [b] -> [b]
get_val    pred           [ ]    [ ]    [ ]    acc =  reverse acc
get_val    pred           (x:xs) (d:ds) [ ]    acc =  get_val pred xs ds [] (d:acc)
get_val    pred           (x:xs) (d:ds) (v:vs) acc
    | pred x     = get_val pred xs ds vs (v:acc)
    | otherwise  = get_val pred xs ds (v:vs) (d:acc)

-- define lenses
x' :: Lens Point Double
x' = (getX, setX)

y' :: Lens Point Double
y' = (getY, setY)

center' :: Lens Circle Point
center' = (getCenter, setCenter)

radius' :: Lens Circle Double
radius' = (getR, setR)

points' :: Lens Surface [Point]
points' = (getPoints, setPoints)

--lens for list
posSet :: Int -> a -> [a] -> [a]
posSet i n xs = let (a,_:b) = splitAt i xs in a ++ [n] ++ b

at' :: Int -> Lens [a] a
at' i = (\xs -> xs !! i, posSet i)

-- get value from object of type 'a' via lens 'l'
(^.) :: a -> Lens a b -> b
a ^. l  = getL l a

-- set value in object if type 'a' via lens 'l'
(^=) :: Lens a b -> b -> a -> a
(l ^= b) a = setL l b a

-- map value from object of type 'a' with function 'f' via lens 'l'
(%=) :: Lens a b -> (b -> b) -> a -> a
(l %= f) a = modL l f a

-- cursor
-- cursor is a pair of lens and data
type Cursor a b = (a, b)

cGet :: Cursor a (Lens a b) -> b
cGet (d, l) = d ^. l

cSet :: Cursor a (Lens a b) -> b -> a
cSet (d, l) v = (l ^= v) d

(|>>) :: Cursor a (Lens a b) -> Lens b c -> Cursor a (Lens a c)
(d, l1) |>> l2 = (d, l1 <.> l2)
