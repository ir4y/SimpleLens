import Prelude hiding ((.), id, const, curry, uncurry)
import Control.Monad.State
import Control.Category

-- Point data type
data Point  = Point  { x :: Double, y :: Double }
    deriving (Show)

getX :: Point -> Double
getX = x

setX :: Double -> Point -> Point
setX x' p = p { x = x' }

getY :: Point -> Double
getY = y

setY :: Double -> Point -> Point
setY y' p = p { y = y' }


--Circle datatype
data Circle = Circle { center :: Point, radius :: Double }
    deriving (Show)

getCenter :: Circle -> Point
getCenter = center

setCenter :: Point -> Circle -> Circle
setCenter center' c = c {center = center'}

getR :: Circle -> Double
getR = radius

setR :: Double -> Circle -> Circle
setR r' c = c {radius = r'}

type Lens a b = ( a -> b , b -> a -> a )

getL :: Lens a b -> a -> b
getL (g, _) = g

setL :: Lens a b -> b -> a -> a
setL (_, s) = s

-- Apply a function to the field
-- I consider modL more fundamental than setL
modL :: Lens a b -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a

(<.>):: Lens y z -> Lens x y -> Lens x z
(getA,setA) <.> (getB,setB) = (getA . getB, \z x -> setB (setA z (getB x)) x)

{-instance Category Lens where-}
    {-id x = x-}
    {-(.) = (<.>)-}

x' :: Lens Point Double
x' = (getX, setX)

y' :: Lens Point Double
y' = (getY, setY)

center' :: Lens Circle Point
center' = (getCenter, setCenter)

radius' :: Lens Circle Double
radius' = (getR, setR)

(^.) :: a -> Lens a b -> b
a ^. p     = getL p a

(^=) :: Lens a b -> b -> a -> a
(p ^= b) a = setL p b a

-- stateful version of (^=)
(^:=) :: Lens a b -> b -> State a ()
p ^:= b = do
    a <- get -- read the current state
    let a' = setL p b a
    put a'   -- set the current state
-- or p ^:= b = modify (setL p b)

access :: Lens a b -> State a b
access p = do
    a <- get -- read the current state
    let b = getL p a
    return b -- return the value
-- or access p = gets (getL p)


-- modify field 'p' using function f
(%=) :: Lens a b -> (b -> b) -> State a ()
p %= f = do
    b <- access p
    p ^:= f b

p += x = p %= (+ x)
p *= x = p %= (* x)

goUp :: State Point ()
goUp = y' += 7.0

goRight = (x' <.> center') += 10