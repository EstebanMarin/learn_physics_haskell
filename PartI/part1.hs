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

f2 :: (Floating a) => a -> a
f2 x = x ** 3

main :: IO ()
-- ghci> main
-- der1: 1.0000000000000002
-- der2: 1.0
-- der3: 1.0
main = do
  putStrLn ("der1: " ++ show (der1 1))
  putStrLn ("der2: " ++ show (der2 1))
  putStrLn ("der3: " ++ show (der3 1))