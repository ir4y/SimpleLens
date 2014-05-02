import Test.HUnit

-- cabal instll HUnit
-- rm -f *.o && rm -f Lens && rm -f *.tix && ghc -fhpc Lens.hs && ./Lens && hpc markup lens
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


-- define lenses
x' :: Lens Point Double
x' = (getX, setX)

y' :: Lens Point Double
y' = (getY, setY)

center' :: Lens Circle Point
center' = (getCenter, setCenter)

radius' :: Lens Circle Double
radius' = (getR, setR)

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

test_composition = TestCase (do { assertEqual "get x' from Circle center'" ((Circle (Point 1.0 2.0) 3.0) ^. (center' <.> x')) 1.0
                                ; assertEqual "set x' of Circle center'" (((center' <.> x') ^= 10.0) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 10.0 2.0) 3.0)
                                ; assertEqual "increase x' of Circle center'" (((center' <.> x') %= (+10.0)) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 11.0 2.0) 3.0)
                                })


tests = TestList [ TestLabel "Test x' lens" test_x'_lens
                 , TestLabel "Test y' lens" test_y'_lens
                 , TestLabel "Test center' lens" test_center'_lens
                 , TestLabel "Test radius' lens" test_radius'_lens
                 , TestLabel "Test composion" test_composition
                 ]

main = runTestTT tests