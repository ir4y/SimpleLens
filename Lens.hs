import Control.Applicative
import Data.List
import Test.HUnit
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


-- Tests 
test_x'_lens = TestCase (do { assertEqual "x' getter" ((Point 1.0 2.0) ^. x') 1.0
                            ; assertEqual "x' setter" ((x' ^= 10) (Point 1.0 2.0)) (Point 10.0 2.0)
                            })

test_y'_lens = TestCase (do { assertEqual "y' getter" ((Point 1.0 2.0) ^. y') 2.0
                            ; assertEqual "y' setter" ((y' ^= 10) (Point 1.0 2.0)) (Point 1.0 10.0)
                            })

test_center'_lens = TestCase (do { assertEqual "center' getter" ((Circle (Point 1.0 2.0) 3.0) ^. center') (Point 1.0 2.0)
                                 ; assertEqual "center' setter" ((center' ^= (Point 10.0 10.0)) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 10.0 10.0) 3.0)
                                 })
 
test_radius'_lens = TestCase (do { assertEqual "radius' getter" ((Circle (Point 1.0 2.0) 3.0) ^. radius') 3.0
                                 ; assertEqual "radius' setter" ((radius' ^= 10.0) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 1.0 2.0) 10.0)
                                 })

test_points'_lens = TestCase (do { assertEqual "points' getter" ((Surface [(Point 1.0 2.0), (Point 3.0 4.0)]) ^. points') [(Point 1.0 2.0), (Point 3.0 4.0)]
                                 ; assertEqual "points' setter" ((points' ^= [(Point 0.0 0.0)]) (Surface [(Point 1.0 2.0), (Point 3.0 4.0)]))  (Surface [(Point 0.0 0.0)])
                                 })
                                   
test_composition = TestCase (do { assertEqual "get x' from Circle center'" ((Circle (Point 1.0 2.0) 3.0) ^. (center' <.> x')) 1.0
                                ; assertEqual "set x' of Circle center'" (((center' <.> x') ^= 10.0) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 10.0 2.0) 3.0)
                                ; assertEqual "increase x' of Circle center'" (((center' <.> x') %= (+10.0)) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 11.0 2.0) 3.0)
                                ; assertEqual "get y' of second Surface point" ((Surface [(Point 0.0 0.0), (Point 1.0 2.0)]) ^. (points' <.>  (at' 1) <.> y')) 2.0
                                ; assertEqual "set y' of second Surface point" (((points' <.>  (at' 1) <.> y') ^= 10.0) (Surface [(Point 0.0 0.0), (Point 1.0 2.0)])) (Surface [(Point 0.0 0.0), (Point 1.0 10.0)])
                                })

test_traversed = TestCase (do { assertEqual "get traversed x' over points'" ((Surface [(Point 0.0 0.0), (Point 1.0 2.0)]) ^. (points' `traversed` x')) [0.0, 1.0]
                              ; assertEqual "set traversed x' over points'" (((points' `traversed` x') ^= [9, 9]) (Surface [(Point 0.0 0.0), (Point 1.0 2.0)])) (Surface [(Point 9.0 0.0), (Point 9.0 2.0)])
                              ; assertEqual "increase traversed x' over points'" (((points' `traversed` x') %= ((<*>) [(+1)])) (Surface [(Point 0.0 0.0), (Point 1.0 2.0)])) (Surface [(Point 1.0 0.0), (Point 2.0 2.0)])
                              })


tests = TestList [ TestLabel "Test x' lens" test_x'_lens
                 , TestLabel "Test y' lens" test_y'_lens
                 , TestLabel "Test center' lens" test_center'_lens
                 , TestLabel "Test radius' lens" test_radius'_lens
                 , TestLabel "Test points' lens" test_points'_lens
                 , TestLabel "Test composion" test_composition
                 , TestLabel "Test traversed" test_traversed
                 ]

main = runTestTT tests
