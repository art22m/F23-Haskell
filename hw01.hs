{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS_GHC -Wall #-}
import CodeWorld

-- Murashko Artem BS20-02

--
-- 1.1 Simple traffic system
--

-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Color -> Double -> Picture
lightCircle c y = translated 0 y (colored c (solidCircle 1))

-- | Green traffic light.
greenLight :: Picture
greenLight = lightCircle green (-1.2)

-- | Red traffic light.
redLight :: Picture
redLight = lightCircle red 1.2

-- | Frame for traffic lights.
frame :: Picture
frame = rectangle 2.5 5

-- | Traffic lights controller switching lights every 2 seconds.
trafficController :: (Bool -> Picture) -> (Double -> Picture)
trafficController tl = go where 
  go t
    | floor (t / 2) `mod` 2 == 0 = tl True
    | otherwise                  = tl False


-- | Simple traffic lights with two states.
-- * 'True' — green light
-- * 'False' — red light
trafficLights :: Bool -> Picture
trafficLights True  = frame <> greenLight
trafficLights False = frame <> redLight

--
-- 1.2 Complex traffic lights
--

-- 1.2.1 Inactive light circles

-- | Draw a circle for traffic lights given color, isDimmed parameter and Y-offset.
complexLightCircle :: Color -> Bool -> Double -> Picture
complexLightCircle c False y = translated 0 y (colored c (solidCircle 1))
complexLightCircle c True y = translated 0 y (colored (darker 0.25 c) (solidCircle 1))

-- | Red traffic light with isDimmed parameter and Y-offset.
complexRedLight :: Bool -> Double -> Picture
complexRedLight isDimmed y = complexLightCircle red isDimmed y

-- | Green traffic light.
complexYellowLight :: Bool -> Double -> Picture
complexYellowLight isDimmed y = complexLightCircle yellow isDimmed y

-- | Green traffic light.
complexGreenLight :: Bool -> Double -> Picture
complexGreenLight isDimmed y = complexLightCircle green isDimmed y

-- | Simple traffic lights with two states and invactive lights.
-- * 'True' — green light
-- * 'False' — red light
trafficLightsWithInactive :: Bool -> Picture
trafficLightsWithInactive isGreen  = frame <> red <> green
  where
    red = complexRedLight isGreen 1.2
    green = complexGreenLight (not isGreen) (-1.2)

-- solution
solution121 ::IO()
solution121 = animationOf (trafficController trafficLightsWithInactive)


-- 1.2.2 Traffic lights for cars

-- | Comlpex traffic lights with four states.
-- * False False — green light active
-- * False True — yellow light active
-- * True False — red light active
-- * True True — red and yellow lights active
complexTrafficLights :: Bool -> Bool -> Picture
complexTrafficLights lhs rhs = largeFrame <> red <> yellow <> green
  where
    largeFrame = rectangle 2.5 7
    red = complexRedLight (not lhs) 2.2
    yellow = complexYellowLight (not rhs) 0
    green = complexGreenLight (lhs || rhs) (-2.2)
-- Explanation (or another approach)
--complexTrafficLights False False  = largeFrame <> complexRedLight True <> complexYellowLight True <> complexGreenLight False
--complexTrafficLights False True   = largeFrame <> complexRedLight True <> complexYellowLight False <> complexGreenLight True
--complexTrafficLights True False   = largeFrame <> complexRedLight False <> complexYellowLight True <> complexGreenLight True
--complexTrafficLights True True    = largeFrame <> complexRedLight False <> complexYellowLight False <> complexGreenLight True

-- | Complex traffic lights controller switching lights.
complexTrafficController :: Double -> Picture
complexTrafficController t
  | tFrame <= 2                = complexTrafficLights False False -- 3 sec
  | tFrame == 3                = complexTrafficLights False True  -- 1 sec
  | 4 <= tFrame && tFrame <= 6 = complexTrafficLights True False  -- 3 sec 
  | otherwise                  = complexTrafficLights True True   -- 1 sec
  where 
    tFrame = floor (t) `mod` 8

-- solution
solution122 ::IO()
solution122 = animationOf complexTrafficController


-- 1.2.3 Pedestrians and cyclists
        
-- | Comlpex traffic lights with four states.
-- * False True — green light active
-- * False False - all lights inactive
-- * True True — all lights active
-- * True False — red light active
complexNoCarTrafficLights :: Picture -> Bool -> Bool -> Picture
complexNoCarTrafficLights img lhs rhs = frame <> red <> green
  where 
    red = complexRedLight (not lhs) 1.2
    green = translated 0 (-1.2) img <> complexGreenLight (not rhs) (-1.2)

complexNoCarTrafficController :: Picture -> (Double -> Picture)
complexNoCarTrafficController img = go where 
  go t
    | tFrame <= 11                 = complexNoCarTrafficLights img False True  -- 3 sec
    | tFrame == 12 || tFrame == 14 = complexNoCarTrafficLights img False False -- 0.5 sec
    | tFrame == 13 || tFrame == 15 = complexNoCarTrafficLights img False True  -- 0.5 sec
    | otherwise                    = complexNoCarTrafficLights img True False  -- 4 sec
    where 
      tFrame = floor (t * 4) `mod` 32

-- solution
solution123_1 ::IO()
solution123_1 = animationOf (complexNoCarTrafficController (lettering "\x1F6B6"))

solution123_2 ::IO()
solution123_2 = animationOf (complexNoCarTrafficController (lettering "\x1F6B2"))


--
-- 2.1 Fractal Tree
--

-- 2.2.1 Continuous growth

-- | A fractal tree of a given rank.
tree :: Double -> Picture
tree t 
  | t <= 0 = blank 
  | otherwise = segment <> translated 0 (min t 1.0) (leftBranch <> rightBranch) 
  where
    segment = polyline [(0, 0), (0, (min t 1.0))]
    leftBranch  = rotated (pi/8) (tree (t - 1))
    rightBranch = rotated (-pi/8) (tree (t - 1))

solution221 ::IO()
solution221 = animationOf tree

leaf :: Double -> Picture 
leaf size = colored green (solidCircle size)

-- 2.2.2 Thick tree
thickTree :: Bool -> (Double -> Picture)
thickTree showLeafs = go where 
  go t  
    | t <= 0 = blank 
    | 0 < t && t <= 1 && showLeafs = branchLeaf <> segment <> translated 0 branchHeight (leftBranch <> rightBranch)
    | otherwise = segment <> translated 0 branchHeight (leftBranch <> rightBranch)
    where
      segment = solidPolygon [((max (-0.1 * t) (-0.1 * 1.5)), 0), ((min (0.1 * t) (0.1 * 1.5)), 0), ((min (0.05 * t) (0.1 * 1.5)), branchHeight), ((max (-0.05 * t) (-0.1 * 1.5)), branchHeight)]
      leftBranch  = rotated (pi/8) (thickTree showLeafs (t - 1))
      rightBranch = rotated (-pi/8) (thickTree showLeafs (t - 1))
      branchHeight = min t 1.5
      branchLeaf = translated 0 branchHeight (leaf 0.1)
  

solution222 ::IO()
solution222 = animationOf (thickTree False)

solution223 ::IO()
solution223 = animationOf (thickTree True)

main ::IO()

-- main = solution121
-- main = solution122
-- main = solution123_1
-- main = solution123_2

main = solution221
-- main = solution222 
-- main = solution223 
