{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

-- Murashko Artem SD20-01

-- | N-body simulation.

module Main where
import CodeWorld

-- * Types

-- | Mass (in kilograms).
type Mass = Double

-- | Position (coordinates in meters).
type Position = (Double, Double)

-- | Velocity vector (components in meters per second).
type Velocity = (Double, Double)

-- | An astronomical body (e.g. a planet or an asteroid).
data Body = Body Mass Position Velocity

-- | A system of bodies.
data System = System [Body]

-- * Rendering

-- | Render a single body, taking visualisation constants into account.
drawBody :: Body -> Picture
drawBody (Body mass (x, y) _ ) = translated x y (colored color (solidCircle radius))
  where
    color = if mass > smallMassThreshold then red else blue
    radius = massToRadius mass


-- | Render a system of bodies on a black background.
drawSystem :: System -> Picture
drawSystem (System bs) = (pictures (map drawBody bs)) <> background
  where
    background = solidRectangle 100 100

-- * Physics (updates)

-- | Update body's position based on its velocity.
moveBody :: Double -> Body -> Body
moveBody dt (Body m (x, y) (vx, vy))
  = Body m (x + dt * vx, y + dt * vy) (vx, vy)


-- | Update body's position and velocity.
updateBody :: Double -> [Body] -> Body -> Body
updateBody dt bodies = moveBody dt . applyGravity dt bodies

-- | Update all bodies in a system.
updateBodies :: Double -> [Body] -> [Body]
updateBodies dt bodies
  = map (moveBody dt . applyGravity dt bodies) bodies

-- | Update entire system, taking 'timeScale' into account.
updateSystem :: Double -> System -> System
updateSystem dt (System bs)
  = System (updateBodies (dt * timeScale) bs)

-- ** Gravity helpers

-- | Acceleration in a two body system.
--
-- NOTE: ignores gravitional effect of "small" objects
gravityAcc
  :: Body     -- ^ Body whose gravitational field is used.
  -> Body     -- ^ Body to compute acceleration for.
  -> Vector
gravityAcc (Body _ (x1, y1) _) (Body m2 (x2, y2) _)
  | r < 1e-6 = (0, 0) -- avoid division by zero
  | otherwise = scaledVector (bigG * m2 / r^3) (vectorDifference (x2, y2) (x1, y1))
  where
     r = vectorLength (vectorDifference (x2, y2) (x1, y1))

-- | Compute and apply acceleration to update body's velocity.
applyGravity :: Double -> [Body] -> Body -> Body
applyGravity dt bodies body = _exercise -- Hint: split into smaller parts, and use vectorSum

-- * Controls

-- | Handle user input (e.g. mouse clicks).
handleSystem :: Event -> System -> System
handleSystem _ = id -- ignore all events

-- * Helpers

-- | Convert pointer position into coordinates in a simulation.
fromPointer :: Point -> Point
fromPointer (x, y) = (x * viewportScale, y * viewportScale)

-- * Constants

-- ** Physical constants

-- | One astronomical unit (in meters).
au :: Double
au = 149597900000

-- | Gravitational constant.
bigG :: Double
bigG = 6.67430e-11

-- ** Visualisation parameters

-- | Viewport scaling factor.
--
-- For inner solar system: 1 unit = 0.2 'au'.
-- For Earth-Moon: 1 unit = 0.0005 'au'.
viewportScale :: Double
viewportScale = _exercise

-- | Time scale for the simulation.
--
-- For inner solar system: 1 second = 1 week
-- For Earth-Moon: 1 second = 1 day
timeScale :: Double
timeScale = _exercise

-- | Mass to visualisation radius mapping.
-- For nicer visualisation we use logarithmic scale.
massToRadius :: Mass -> Double
-- massToRadius m = 0.01 + 3e-7 * (logBase 10 (m + 10)) ^ 4
massToRadius m = sqrt m

-- | Smallest mass to take gravity into account for.
smallMassThreshold :: Mass
smallMassThreshold = 1e21
 

test1 :: System
test1 = System [Body 20 (0, 0) (0, 0), Body 10 (10, 0) (0, 0), Body 5 (17, 0) (0, 0)]

main :: IO ()
main = drawingOf (drawSystem test1)