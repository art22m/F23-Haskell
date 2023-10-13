{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
import CodeWorld

-- Murashko Artem BS20-02

-- Config

worldSize :: Int
worldSize = 21

-- # 1.1 Limited movement 

data Dir = U | R | D | L 

move :: Dir -> Coords -> Coords
move U (i, j) = (i, j + 1)
move R (i, j) = (i + 1, j)
move D (i, j) = (i, j - 1)
move L (i, j) = (i - 1, j)

canMove :: Tile -> Bool
canMove Floor = True
canMove Exit = True
canMove _ = False

tryMove :: Dir -> Coords -> Coords
tryMove dir coords = newCoords
  where
    movedCoords = move dir coords
    movedCoordsTile = levelMap movedCoords
    newCoords
      | canMove movedCoordsTile = movedCoords
      | otherwise = coords
    
renderWorldSolution1 :: Coords -> Picture
renderWorldSolution1 _coords = (renderCharacter _coords) <> drawRows levelMap (1, 7) (1, 7)

solution1 :: IO()
solution1 = activityOf initialWorld handleWorld renderWorldSolution1

-- # 1.2 Draw any level map #

-- | Draw a given level map of size 21x21.
drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap mp = drawRows mp (1, 21) (1, 21)

renderWorldSolution2 :: Coords -> Picture
renderWorldSolution2 _coords = translated (-(fromIntegral worldSize) / 2)  (-(fromIntegral worldSize) / 2) 
                        (scaled 0.8 0.8 
                           ((renderCharacter _coords) <> drawLevelMap levelMap)
                        )
solution2 :: IO()
solution2 = activityOf initialWorld handleWorld renderWorldSolution2

-- # 1.3 Open doors #

-- | Open some doors.
openDoors :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile) 
openDoors colors mp = newMp
  where
    newMp coords = doorLevelMap coords colors mp
    
doorLevelMap :: Coords -> [DoorColor] -> (Coords -> Tile) -> Tile
doorLevelMap coords colors mp = mappedTile
  where
    tile = mp coords
    mappedTile
      | isOpenedDoor tile colors = Floor
      | otherwise = tile
    
isOpenedDoor :: Tile -> [DoorColor] -> Bool
isOpenedDoor (Door dc) colors = oneOf dc colors
isOpenedDoor _ _ = False


oneOf :: DoorColor -> [DoorColor] -> Bool
oneOf color colors = color `elem` colors

renderWorldSolution3 :: Coords -> Picture
renderWorldSolution3 _coords = translated (-(fromIntegral worldSize) / 2)  (-(fromIntegral worldSize) / 2) 
                        (scaled 0.8 0.8 
                           ((renderCharacter _coords) <> drawLevelMap (openDoors [] levelMap))
                        )
solution3 :: IO()
solution3 = activityOf initialWorld handleWorld renderWorldSolution3

-- Helpers 

-- Map Generation
drawTileAt :: (Int, Int) -> Tile -> Picture
drawTileAt (x, y) tt = translated (fromIntegral x) (fromIntegral y) (drawTile tt)
                                        

drawRow :: (Coords -> Tile) -> Int -> (Int, Int) -> Picture
drawRow mp fromX (fromY, toY)
  | fromY > toY = blank
  | otherwise = drawTileAt (fromX, fromY) (mp (fromX, fromY)) <> drawRow mp fromX (fromY + 1, toY)


drawRows :: (Coords -> Tile) -> (Int, Int) -> (Int, Int) -> Picture
drawRows mp (fromX, toX) (fromY, toY)
  | fromX > toX = blank
  | otherwise = drawRow mp fromX (fromY, toY) 
      <> drawRows mp (fromX + 1, toX) (fromY, toY)
  
-- Tiles

tile :: Picture
tile = solidRectangle 1 1

wallTile :: Picture
wallTile = colored black tile

floorTile :: Picture
floorTile = colored grey tile

doorTile :: Color -> Picture
doorTile c = colored c tile

exitTile :: Picture
exitTile = colored yellow tile

player :: Picture 
player = lettering "\x1F6B6"

data DoorColor = Red | Blue | Green deriving (Eq)

doorColor :: DoorColor -> Color
doorColor Red = red
doorColor Blue = blue
doorColor Green = green

data Tile = Wall | Floor | Door DoorColor | Exit

drawTile :: Tile -> Picture 
drawTile Wall = wallTile
drawTile Floor = floorTile
drawTile (Door dc) = doorTile (doorColor dc)
drawTile Exit = exitTile

type Coords = (Int, Int)

levelMap :: Coords -> Tile
levelMap (i, j)
  | i == j + 9 = Wall
  | i == j - 11 = Wall
  | i == 10 = Wall
  | (i, j) == (6, 6) = Door Blue
  | (i, j) == (3, 3) = Door Red
  | i == 1 || i == worldSize = Wall
  | j == 1 || j == worldSize = Wall
  | otherwise = Floor


-- activityOf components 

initialWorld :: Coords
initialWorld = (5, 5)

handleWorld :: Event -> Coords -> Coords
handleWorld (TimePassing dt) coords = updateWorld dt coords
handleWorld (KeyPress "Up") coords = tryMove U coords
handleWorld (KeyPress "Down") coords = tryMove D coords
handleWorld (KeyPress "Left") coords = tryMove L coords
handleWorld (KeyPress "Right") coords = tryMove R coords
handleWorld _anyEvent coords = coords

updateWorld :: Double -> Coords -> Coords
updateWorld _dt = id

renderCharacter :: Coords -> Picture
renderCharacter (i, j) = translated (fromIntegral i) (fromIntegral j) player


-- Start Game
main ::IO()
-- main = solution1
-- main = solution2
main = solution3