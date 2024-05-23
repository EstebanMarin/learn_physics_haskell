-- import Control.Monad.RWS (MonadState (put))

import Data.Maybe (maybeToList)
import Graphics.Gnuplot.Simple

-- Describing Motion

type R = Double

type Time = R

type TimeInterval = R

type Velocity = R

type Position = R

type Acceleration = R

type PositionFunction = Time -> Position

type VelocityFunction = Time -> Velocity

type AccelerationFunction = Time -> Acceleration

type Derivate = (R -> R) -> R -> R

-- derivative implementation using the definition of a derivative

derivate :: R -> Derivate
derivate dt x t = (x (t + dt / 2) - x (t - dt / 2)) / dt

-- the compile sees these as R -> (R -> R) -> R -> R or R -> derivativate this is a type derivative
-- seeing a derivative as something that takes a function and an input and returns a function
-- in this case it takes derivate takes a time interval and a position function and returns a velocity function

-- modeling a car position

carPosition :: PositionFunction
carPosition = cos

-- using the derivative function to get the velocity function

carVelocity :: VelocityFunction
carVelocity = derivate 0.01 carPosition

---
velFromPost :: R -> PositionFunction -> VelocityFunction
velFromPost = derivate

positionCV :: Position -> Velocity -> Time -> Position
positionCV x0 v0 t = x0 + v0 * t

-- modeling acceleration

accFromVel :: R -> VelocityFunction -> AccelerationFunction
accFromVel = derivate

velocityCA :: Velocity -> Acceleration -> VelocityFunction
velocityCA v0 a0 t = v0 + a0 * t

positionCA :: Position -> Velocity -> Acceleration -> Time -> Position
positionCA x0 v0 a0 t = x0 + v0 * t + 0.5 * a0 * t ** 2

-- exercise 4.1
f1 :: (Floating a) => a -> a
f1 x = 0.5 * x ** 2

der1 = derivate 0.1 f1

der2 = derivate 1 f1

der3 = derivate 10 f1

-- excerise 4.2

analitycDerivate :: (Floating a) => a -> a
analitycDerivate x = x ** 3

symbolicDerivate :: (Floating a) => a -> a
symbolicDerivate x = 3 * x ** 2

der4 = derivate 1 analitycDerivate

main :: IO ()
-- ghci> main
-- der1: 1.0000000000000002
-- der2: 1.0
-- der3: 1.0

-- velocities
velocities :: [R]
velocities = [0, -9.8, -19.6, -29.4, -39.2, -49, -58.8, -68.6, -78.4, -88.2, -98]

moreVelocities :: [R]
moreVelocities = [0, -9.8, -19.6, -29.4, -39.2, -49, -58.8, -68.6, -78.4, -88.2, -98]

ns :: [R]
ns = [0 .. 10]

-- list typeS
funcs :: [R -> R]
funcs = [cos, sin, tan, exp, log, sqrt, (** 2), (** 3), (** 4), (** 5)]

-- list function
sumVelocities :: [R]
sumVelocities = velocities ++ moreVelocities

ts :: [R]
ts = [0, 0.1 .. 6]

yRock30 :: R -> R
yRock30 t = 30 * t - 0.5 * 9.8 * t ** 2

yRock :: R -> R -> R
yRock v0 t = v0 * t - 0.5 * 9.8 * t ** 2

xRock :: R -> R -> R
xRock v0 t = v0 * t

xs :: [R]
xs = [yRock30 t | t <- ts]

-- constructors and pattern matiching

secondItem :: [a] -> a
secondItem ys = case ys of
  [] -> error "empty list"
  (x : xs) -> if null xs then error "list too short" else head xs

secondItem2 :: [a] -> a
secondItem2 [] = error "empty list"
secondItem2 (x : xs) = if null xs then error "list too short" else head xs

secondItem3 :: [a] -> a
secondItem3 ys = case ys of
  [] -> error "empty list"
  [x] -> error "list too short"
  (x : xs) -> head xs

secondItem4 :: [a] -> a
secondItem4 [] = error "empty list"
secondItem4 [x] = error "list too short"
secondItem4 (x : xs) = head xs

-- exercise 5.5

null' :: [a] -> Bool
null' ys = length ys == 0

-- null' = null

-- suggests native null'

-- exercise 5.12

expression :: (Floating a) => a -> a
expression n = 1 / (n ** 2)

evaluateTill :: [Double]
evaluateTill = [1 .. 100]

euler :: Double
euler = sum [1 / (n ** 2) | n <- evaluateTill]

factProc :: (Num a, Enum a) => a -> [a]
factProc n = [product [1 .. n] | n <- [1 .. n]]

factTest :: (Num a, Enum a) => a -> a
factTest n = last $ [product [1 .. n] | n <- [1 .. n]]

expFunction :: (Enum a, Floating a) => p -> [a]
expFunction n = [(1 + 1 / n) ** n | n <- [0 ..]]

limit :: (Enum a, Ord a, Fractional a, Floating a) => a -> a
limit epsilon = head [x | (x, y) <- zip sequence (tail sequence), abs (x - y) < epsilon]
  where
    sequence = [(1 + 1 / n) ** n | n <- [1 ..]]

test1 = take 1 $ expFunction 1000

test10 = take 10 $ expFunction 1000

--  function to integratelower bound, upper bound, result
type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
-- using midpoint rule
integral dt f a b = sum [f (t * dt) | t <- [a + dt / 2, a + 3 * dt / 2 .. b - dt / 2]]

-- ghci> integral 0.01 (\x -> 1 / (x **2)) 0 1
-- 4.9248022838750976e8

-- lets implement anti derivates
-- initial value, function, function
type Antiderivative = R -> (R -> R) -> (R -> R)

antiderivate :: R -> Antiderivative
antiderivate dt v0 a t = v0 + integral dt a 0 t

velFromAcc :: R -> Velocity -> AccelerationFunction -> VelocityFunction
-- velFromAcc dt v0 a t = antiderivate dt v0 a t
velFromAcc = antiderivate

postFromVel :: R -> Position -> VelocityFunction -> PositionFunction
postFromVel = antiderivate

plot :: IO ()
plot = plotFunc [] ([0, 0.1 .. 10] :: [Double]) cos

plotx :: IO ()
plotx = plotFunc [] ([0, 0.1 .. 6] :: [Double]) (xRock 30)

-- Execercise 8.1
test :: (Floating a, Enum a) => a -> a
test n = undefined

-- yes but we can create a problematic no way we can make it compile
test2 :: (Floating a, Integral a) => a -> a
test2 n = undefined

ploty :: IO ()
ploty = plotFunc [] ([0, 0.1 .. 6] :: [Double]) (yRock 30)

-- ghci> :i Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--         -- Defined in ‘GHC.Num’
-- instance Num Double -- Defined in ‘GHC.Float’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’

-- Numerical Integration

oneStep :: R -> (R -> R) -> (R, R) -> (R, R)
oneStep dt f (t, x) = (t + dt, x + f t * dt)

integral' :: R -> Integration
integral' dt f a b =
  snd $ head $ dropWhile (\(t, _) -> t < b) $ iterate (oneStep dt f) (a + dt / 2, 0)

-- exercise 9.1
polarToCart :: (R, R) -> (R, R)
polarToCart (r, theta) = (r * cos theta, r * sin theta)

-- exercise 9.3
headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x : _) = Just x

-- exercise 9.4
maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just x) = [x]

-- exercise 9.6

zip' :: ([a], [b]) -> [(a, b)]
zip' (xs, ys) = zip xs ys

-- exercise 9.10
-- time velocity pairs tvPair

tvPairs :: [(R, R)]
tvPairs = iterate tvUpdate (0, 0)

tvUpdate :: (R, R) -> (R, R)
tvUpdate (t, v) = (t + 1, yRock v t)

-- ghci> take 10 tvPairs
-- [(0.0,0.0),(1.0,0.0),(2.0,-4.9),(3.0,-29.400000000000002),(4.0,-132.3),(5.0,-607.6),(6.0,-3160.5),(7.0,-19139.4),(8.0,-134215.90000000002),(9.0,-1074040.8000000003)]

-- exercise 9.11

fibonnaci :: [Int]
fibonnaci = 0 : 1 : zipWith (+) fibonnaci (tail fibonnaci)

fibHelper :: [(Int, Int)]
fibHelper = zip [0 ..] fibonnaci

-- ghci> take 10 fibHelper
-- [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5),(6,8),(7,13),(8,21),(9,34)]

-- exercise 9.12'
-- factorial' :: Int -> Int
-- factorial' n = iterate (\(x, y) -> (x + 1, y * (x + 1))) (0, 1) !! n

factorialHelp :: [(Int, Int)]
factorialHelp = iterate (\(x, y) -> (x + 1, y * (x + 1))) (0, 1)

factorialH :: Int -> Int
factorialH n = snd $ factorialHelp !! n

-- ghci> take 10 factorialHelp
-- [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120),(6,720),(7,5040),(8,40320),(9,362880)]

main = do
  putStrLn "Part I"
