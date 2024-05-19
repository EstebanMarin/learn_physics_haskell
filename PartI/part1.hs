import Control.Monad.RWS (MonadState (put))

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
sum = velocities ++ moreVelocities

ts :: [R]
ts = [0, 0.1 .. 6]

yRock30 :: R -> R
yRock30 t = 30 * t - 0.5 * 9.8 * t ** 2

xs :: [R]
xs = [yRock30 t | t <- ts]

main = do
  -- putStrLn ("der1: " ++ show (der1 1))
  -- putStrLn ("der2: " ++ show (der2 1))
  -- putStrLn ("der3: " ++ show (der3 1))
  putStrLn ("der4: " ++ show (der4 1))
