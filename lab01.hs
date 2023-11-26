{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
import CodeWorld


-- | Cartesian coordinates on a plane.
type Position = (Double, Double)

-- | Position on an ellipse with radius r (half diameter)
-- and angular position theta.
positionOnEllipse :: Double -> Double -> Position
positionOnEllipse r theta = (x', y')
  where
    twist = 1.5         -- how much spiral arms should twist
    prominance = 1.2    -- how elongated are the elliptical orbits
                        -- (also how prominent the sprirals will look)

    -- convert polar coordinates to cartesian
    x = r * cos theta / prominance    -- we shrink the X axis
    y = r * sin theta

    -- convert back to polar
    r' = sqrt (x^2 + y^2)
    theta' = atan2 y x + r * twist    -- with an added twist

    -- convert again to cartesian
    x' = r' * cos theta'
    y' = r' * sin theta'


-- # 1.1 RENDER ELLIPSES # --

ellipse :: Double -> Double -> [Position]
ellipse r th 
  | th < 0 = (positionOnEllipse r 0):[]
  | otherwise = (positionOnEllipse r th) : (ellipse r (th - 0.05))

-- | Draw an elliptical orbit of a given radius.
orbit :: Double -> Picture
orbit r = polyline (ellipse r (pi * 2))

-- | Draw elliptical orbits in
-- a range of radii with a given step.
orbits :: (Double, Double) -> Double -> Picture
orbits (minR, maxR) step
  | minR >= maxR = orbit minR
  | otherwise = (orbit minR) <> (orbits (minR + step, maxR) step)

-- # 1.2 STARS #--

-- | A star. For now determined only by its position.
type Star = Position

-- | A picture of a single star.
star :: Picture
star = (solidCircle 0.07)

-- | Draw stars on every elliptical orbit in
-- a range of radii with a given step.
galaxyStars :: (Double, Double) -> Double -> [Star]
galaxyStars (minR, maxR) step  
  | minR >= maxR = (ellipse minR (pi * 2))++[] 
  | otherwise = (ellipse minR (pi * 2)) ++ (galaxyStars (minR + step, maxR) step)
  
drawStars :: [Star] -> Picture
drawStars [] = blank
drawStars ((x,y):[]) = translated x y star
drawStars ((x,y):sts) = translated x y star <> drawStars sts

-- # 1.3 ANIMATING GALAXY # --
ellipsePhased :: Double -> Double -> Double -> [Position]
ellipsePhased phase r th 
  | th <= 0 = (positionOnEllipse r (0 + phase)):[]
  | otherwise = (positionOnEllipse r (th + phase)) : (ellipsePhased phase r (th - 0.05))

-- | Draw stars on every elliptical orbit
-- in a range of radii with a given step.
galaxyStarsAt :: Double -> (Double, Double) -> Double -> [Star]
galaxyStarsAt phase (minR, maxR) step
  | minR >= maxR = (ellipsePhased phase minR (pi * 2)) ++ [] 
  | otherwise = (ellipsePhased phase minR (pi * 2)) ++ (galaxyStarsAt phase (minR + step, maxR) step)
  
galaxy :: Double -> Picture
galaxy _time = drawStars (galaxyStarsAt _time (0.1, 5) 0.1)

--------
main :: IO ()
-- main = drawingOf (orbits (10, 20) 1)
-- main = drawingOf (orbits (0.1, 5) 0.1)
-- main = drawingOf (drawStars (galaxyStars (0.1, 5) 0.1))
main = animationOf galaxy