-- Murashko Artem BS20-02
-- Lab 7

-- _ + _ ∗ _ = N
-- solve :: Int -> [(Int, Int, Int)]
-- solve n = _

-- 1.1 Applicative brute force

-- 1.2 Monadic brute force

-- 1.3 List comprehension

solve :: Int -> [(Int, Int, Int)]
solve n =
  [ (x, y, z) 
  | x <- [0 .. n], y <- [0 .. n], z <- [0 .. n], 
  x + y * z == n
  ]