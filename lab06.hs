{-# LANGUAGE DeriveFunctor #-}

-- Murashko Artem BS20-02
-- Lab 6

-- | 1 Moving bodies | --

-- type Point = (Float, Float)
-- type Vector = (Float, Float)

-- -- | An asteroid.
-- data Asteroid = Asteroid
-- { asteroidPosition :: Point -- ˆ Asteroid location.
-- , asteroidVelocity :: Vector -- ˆ Asteroid velocity vector.
-- , asteroidSize :: Float -- ˆ Asteroid size.
-- } deriving (Eq, Show)

-- -- | A rotating satellite.
-- data Satellite = Satellite
-- { satellitePosition :: Point -- ˆ Sattelite location.
-- , satelliteVelocity :: Vector -- ˆ Sattelite velocity vector.
-- , satelliteAngle :: Float -- ˆ Sattelite current angle of rotation.
-- } deriving (Eq, Show)

-- -- | An unidentified flying object.
-- data UFO = UFO
-- { ufoPosition :: Point -- ˆ UFO location.
-- , ufoSpeed :: Float -- ˆ UFO speed (in the direction of its target).
-- , ufoTarget :: Point -- ˆ UFO target that it pursues.
-- } deriving (Eq)

-- moveAsteroid :: Float -> Asteroid -> Asteroid
-- moveAsteroid dt = ...

-- moveSatellite :: Float -> Satellite -> Satellite
-- moveSatellite dt = ...

-- moveUFO :: Float -> UFO -> UFO
-- moveUFO dt = ...

-- | 2 Kinds and Functors | --

data I a = I a
-- Kind: * -> *
instance Functor I where
    fmap f (I x) = I (f x)

--  fmap id = id:
--  fmap id (I x) = I (id x) = I x

-- fmap f . fmap g = fmap (f . g):
-- (fmap f . fmap g) (I x) = fmap f (fmap g (I x)) = fmap f (I (g x)) = I (f (g x))
-- fmap (f . g) (I x) = I ((f . g) x) = I (f (g x))

data S a = S a (S a)
-- Kind: * -> *
instance Functor S where 
    fmap f (S a a') = S (f a) (fmap f a')

data T a b = T a (T b a) | N
-- Kind: * -> * -> *
instance Functor (T a) where 
    fmap f N = N 
    fmap f (T l (T l' r')) = T l (T (f l') (fmap f r'))

data C a b = C a
-- Kind: * -> k -> *
instance Functor (C a) where 
    fmap f (C a) = C a

data P a = P
-- Kind: k -> *
instance Functor P where 
    fmap f P = P

data U = U
-- Kind: *
-- Impossible to implement functor since U has kind *

-- | 3 More Functors | --

data AssocList k v = AssocList [(k, v)]
    deriving (Show)
instance Functor (AssocList k) where
    fmap f (AssocList xs) = AssocList (map (\(l,r) -> (l, f r)) xs)

test1 :: AssocList Int Int
test1 = AssocList [(1, 1), (2, 2), (3, 3)]

----------

data RoseTree a = RoseTree a [RoseTree a]
instance Functor RoseTree where 
    fmap f (RoseTree x xs) = RoseTree (f x) (map (fmap f) xs)

----------

data Grid a = Grid [[Grid a]]
instance Functor Grid where 
    fmap f (Grid xs) = Grid (map (map (fmap f)) xs)

    
