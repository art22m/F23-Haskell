{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.List
import Data.Maybe

-- Murashko Artem BS20-02
-- HW5

-- | 1 Tic-Tac-Toe on CodeWorld | --

-- | A mark for tic-tac-toe.
data Mark = X | O
  deriving (Eq, Show)
  
-- | A cell is either empty or has a mark.
type Cell = Maybe Mark

-- | A board is a 2D grid of cells.
type Board = [[Cell]]

-- | Sample 5x4 board.
sampleBoard :: Board
sampleBoard =
  [ [ x, o, n, o, n ]
  , [ n, o, n, x, o ]
  , [ x, n, x, n, n ]
  , [ n, o, n, x, x ] ]
    where
      (o, x, n) = (Just O, Just X, Nothing)
  
-- | Initialise an empty NxM board.
initBoard :: (Int, Int) -> Board
initBoard (n, m) = replicate m (replicate n Nothing)
    
-- | Determine a winner in a game of tic-tac-toe (if exists).
winner :: Board -> Maybe Mark
winner board = getWinner (filter isLongStreak (concatMap streaks allLines))
  where
    allLines = rows ++ columns ++ diagonals
    rows = board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . transpose
    
-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: Eq a => [Maybe a] -> [(Int, a)]
streaks [] = []
streaks (Nothing : xs) = streaks xs
streaks (Just x : xs) = (1 + length ys, x) : streaks zs
  where
    (ys, zs) = span (== Just x) xs
    
-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: (Int, a) -> Bool
isLongStreak (i, _) = i >= 3

-- | Get a winning mark (if exists).
getWinner :: [(Int, a)] -> Maybe a
getWinner = listToMaybe . map snd
    
-- | Draw a rectangular board.
drawBoard :: Board -> Picture
drawBoard brd = renderRenderedRows (map renderRow reversedBrd) blank
  where 
    reversedBrd = reverse brd 
    
    renderRenderedRows :: [Picture] -> Picture -> Picture
    renderRenderedRows pics pic = foldr go pic pics
      where 
        go :: Picture -> Picture -> Picture
        go p1 p2 = (translated 0 1 p2) <> p1
    
    -- (a -> b) -> [a] -> [b]
    -- a = [Cell], b = Picture
    renderRow :: [Cell] -> Picture
    renderRow cells = foldr go blank cells 
      where 
        go :: Cell -> Picture -> Picture 
        go c p = (translated 1 0 p) <> (drawCell c)
          
-- | Draw a single mark (X or O).
drawMark :: Mark -> Picture
drawMark X = scaled 0.4 0.4 (rotated (pi/4) (solidRectangle 0.5 2 <> solidRectangle 2 0.5))
drawMark O = thickCircle 0.2 0.3

-- | Draw one board cell at given coordinates.
drawCell :: Cell -> Picture
drawCell cell = rectangle 1 1 <> cellPicture
  where
    cellPicture =
      case cell of
        Nothing -> blank
        Just m -> drawMark m

-- 1.2 Putting marks

-- | Try place a mark a given position on a board.
-- The type of mark is determined automatically.
-- When the game is over (a winner exists) no marks are placed.
putMarkAt :: (Int, Int) -> Board -> Board
putMarkAt coords brd'
  | winner brd' == Nothing = go coords brd'
  | otherwise = brd'
  where 
    go :: (Int, Int) -> Board -> Board
    go (x, y) brd
      | (x >= 0 && y >= 0 && x < lenX && y < lenY) = updateAt (lenY - y) updateRow brd
      | otherwise = brd
      where 
        lenX = length $ head brd
        lenY = length brd

        updateRow :: [Cell] -> [Cell]
        updateRow row = updateAt (x + 1) updateIfNeeded row

        updateIfNeeded :: Cell -> Cell 
        updateIfNeeded Nothing = getNextMark brd
        updateIfNeeded c = c
    
    
-- | Determine next mark
getNextMark :: Board -> Cell
getNextMark brd 
  | (calculateNonEmpty brd) `mod` 2 == 0 = Just X
  | otherwise = Just O
  
calculateNonEmpty :: Board -> Int 
calculateNonEmpty brd = foldr (+) 0 (map go brd)
  where 
    go :: [Cell] -> Int
    go cells = foldr (+) 0 (map oneIfNonEmpty cells)
    
    oneIfNonEmpty :: Cell -> Int 
    oneIfNonEmpty Nothing = 0
    oneIfNonEmpty _ = 1
  
-- | Try update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt pos fun lst = (init ls) ++ [fun (last ls)] ++ rs
  where 
    (ls, rs) = splitAt pos lst

-- | Handle mouse clicks to put marks.
handleGame :: Event -> Board -> Board
handleGame (PointerPress mouse) = putMarkAt (pointToCoords mouse)
handleGame _ = id

-- | Convert mouse position into board coordinates.
pointToCoords :: Point -> (Int, Int)
pointToCoords (x, y) = (round x, round y)

-- | Run a tic-tac-toe game with a sample starting board.
ticTacToe :: Board -> IO ()
ticTacToe brd = activityOf brd handleGame drawBoard
    
main ::IO()
main = ticTacToe $ initBoard (5, 5)
