{-# OPTIONS -Wall #-}

module SimpleVec where

infixl 6 ^+^

infixl 6 ^-^

infixr 7 *^

infixl 7 ^*

infixr 7 ^/

infixr 7 <.>

infixl 7 ><

type VecDerivative = (R -> Vec) -> R -> Vec

vecDerivative :: R -> VecDerivative
vecDerivative dt v t = (v (t + dt / 2) ^-^ v (t - dt / 2)) ^/ dt

v1 :: R -> Vec
v1 t = 2 *^ t ** 2 *^ iHat ^+^ 3 *^ t ** 3 *^ jHat ^+^ t ** 4 *^ kHat

xCompFunc :: (R -> Vec) -> R -> R
xCompFunc v t = xComp (v t)

type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2) - x (t - dt / 2)) / dt

type Time = R

type PosVec = Vec

type Velocity = Vec

type Acceleration = Vec

velFromPos ::
  R -> -- dt
  (Time -> PosVec) -> -- position function
  (Time -> Velocity) -- velocity function
velFromPos = vecDerivative

accFromVel ::
  R -> -- dt
  (Time -> Velocity) -> -- velocity function
  (Time -> Acceleration) -- acceleration function
accFromVel = vecDerivative

positionCV :: PosVec -> Velocity -> Time -> PosVec
positionCV r0 v0 t = v0 ^* t ^+^ r0

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = a0 ^* t ^+^ v0

positionCA ::
  PosVec ->
  Velocity ->
  Acceleration ->
  Time ->
  PosVec
positionCA r0 v0 a0 t = 0.5 *^ t ** 2 *^ a0 ^+^ v0 ^* t ^+^ r0

aParallel :: Vec -> Vec -> Vec
aParallel v a =
  let vHat = v ^/ magnitude v
   in (vHat <.> a) *^ vHat

aPerp :: Vec -> Vec -> Vec
aPerp v a = a ^-^ aParallel v a

speedRateChange :: Vec -> Vec -> R
speedRateChange v a = (v <.> a) / magnitude v

radiusOfCurvature :: Vec -> Vec -> R
radiusOfCurvature v a = (v <.> v) / magnitude (aPerp v a)

projectilePos :: PosVec -> Velocity -> Time -> PosVec
projectilePos r0 v0 = positionCA r0 v0 (9.81 *^ negateV kHat)

type R = Double

newtype Mass = Mass R deriving (Eq, Show)

data Grade = Grade String Int
  deriving (Eq, Show)

grades :: [Grade]
grades =
  [ Grade "Albert Einstein" 89,
    Grade "Isaac Newton" 95,
    Grade "Alan Turing" 91
  ]

data GradeRecord = GradeRecord
  { name :: String,
    grade :: Int
  }
  deriving (Eq, Show)

gradeRecords1 :: [GradeRecord]
gradeRecords1 =
  [ GradeRecord "Albert Einstein" 89,
    GradeRecord "Isaac Newton" 95,
    GradeRecord "Alan Turing" 91
  ]

gradeRecords2 :: [GradeRecord]
gradeRecords2 =
  [ GradeRecord {name = "Albert Einstein", grade = 89},
    GradeRecord {name = "Isaac Newton", grade = 95},
    GradeRecord {name = "Alan Turing", grade = 91}
  ]

data MyBool = MyFalse | MyTrue
  deriving (Eq, Show)

data MyMaybe a
  = MyNothing
  | MyJust a
  deriving (Eq, Show)

data Vec = Vec
  { xComp :: R, -- x component
    yComp :: R, -- y component
    zComp :: R -- z component
  }
  deriving (Eq)

instance Show Vec where
  show (Vec x y z) =
    "vec "
      ++ showDouble x
      ++ " "
      ++ showDouble y
      ++ " "
      ++ showDouble z

showDouble :: R -> String
showDouble x
  | x < 0 = "(" ++ show x ++ ")"
  | otherwise = show x

-- Form a vector by giving its x, y, and z components.
vec ::
  R -> -- x component
  R -> -- y component
  R -> -- z component
  Vec
vec = Vec

iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

zeroV :: Vec
zeroV = vec 0 0 0

negateV :: Vec -> Vec
negateV (Vec ax ay az) = Vec (-ax) (-ay) (-az)

(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz = Vec (ax + bx) (ay + by) (az + bz)

(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax - bx) (ay - by) (az - bz)

sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroV

(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = Vec (c * ax) (c * ay) (c * az)

(^*) :: Vec -> R -> Vec
Vec ax ay az ^* c = Vec (c * ax) (c * ay) (c * az)

(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax * bx + ay * by + az * bz

(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz =
  Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax / c) (ay / c) (az / c)

magnitude :: Vec -> R
magnitude v = sqrt (v <.> v)

vecIntegral ::
  R -> -- step size dt
  (R -> Vec) -> -- vector-valued function
  R -> -- lower limit
  R -> -- upper limit
  Vec -- result
vecIntegral = undefined

maxHeight :: PosVec -> Velocity -> R
maxHeight = undefined

speedCA :: Velocity -> Acceleration -> Time -> R
speedCA = undefined

xyProj :: Vec -> Vec
xyProj = undefined

magAngles :: Vec -> (R, R, R)
magAngles = undefined

gEarth :: Vec
gEarth = undefined

vBall :: R -> Vec
vBall = undefined

speedRateChangeBall :: R -> R
speedRateChangeBall = undefined

rNCM :: (R, R -> R) -> R -> Vec
rNCM (radius, theta) = undefined radius theta

aPerpFromPosition :: R -> (R -> Vec) -> R -> Vec
aPerpFromPosition epsilon r t =
  let v = vecDerivative epsilon r
      a = vecDerivative epsilon v
   in aPerp (v t) (a t)