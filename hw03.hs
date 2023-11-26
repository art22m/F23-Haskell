{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Murashko Artem BS20-02
-- Homework 3

-- | 1 Simple Elevator | --

-- 1.1 Buttons

-- Helpers 

button :: Picture
button = (colored (light gray) $ solidCircle 0.9) <> solidCircle 1

arrow :: Picture
arrow = colored white $ solidPolygon [(-0.5, -0.5), (0, 0.5), (0.5, -0.5), (0, 0)]

-- | Elevator button.
data Button = GoUp | GoDown | Stop

-- | Render elevator button.
drawButton :: Button -> Picture
drawButton GoUp = arrow <> button 
drawButton GoDown = rotated pi arrow <> button
drawButton Stop = (scaled 0.5 0.5 $ colored red $ lettering "STOP") <> button

-- | Draw several objects
-- some distance apart from each other. 
asSpaced
  :: Double         -- ˆ How far apart to draw objects. 
  -> (a -> Picture) -- ˆ How to draw a single object. 
  -> [a]            -- ˆ A list of objects to draw.
  -> Picture
asSpaced _ _ [] = blank
asSpaced offset draw (x:xs) = draw x 
  <> (translated offset 0 $ asSpaced offset draw xs)
  
-- Solution 1
solution1 :: IO()
solution1 = drawingOf $ asSpaced 3 drawButton [GoUp, GoDown, Stop]


-- 1.2 Elevator mode

-- Helpers

grayRect :: Picture
grayRect = colored (light gray) (solidRectangle 2 4) <> (solidRectangle 2.1 4.1)

redArrow :: Picture
redArrow = colored red arrow

drawArrowsOnRect :: Picture -> Picture -> Picture
drawArrowsOnRect ar1 ar2 = 
  (translated 0 1 ar1) <> (translated 0 (-1) $ rotated pi ar2) <> grayRect

-- | Elevator mode of operation.
data Mode = Idle | MovingUp | MovingDown

-- | Render elevator mode.
drawMode :: Mode -> Picture
drawMode Idle = drawArrowsOnRect arrow arrow 
drawMode MovingUp = drawArrowsOnRect redArrow arrow
drawMode MovingDown = drawArrowsOnRect arrow redArrow 

-- | FSM corresponding to a simple elevator.
elevator :: Mode -> [(Button, Mode)]
elevator Idle = [(GoUp, MovingUp), (GoDown, MovingDown)]
elevator MovingUp = [(Stop, Idle)]
elevator MovingDown = [(Stop, Idle)]

-- | Apply an action (if any) to the current state 
-- of a finite state machine.
applyAction 
  :: Maybe a          -- ˆ An action to apply (if any).
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)])  -- ˆ State transitions.
  -> s                -- ˆ Current state.
  -> s                -- ˆ New state (if possible).
applyAction Nothing _ _ st = st
applyAction (Just b) equalityTest tr st = 
  case filter (\(b', _) -> equalityTest b b') (tr st) of
    [] -> st
    (_, nst):_ -> nst

-- equality test
buttonsEqualityTest :: Button -> Button -> Bool
buttonsEqualityTest Stop Stop = True
buttonsEqualityTest GoUp GoUp = True
buttonsEqualityTest GoDown GoDown = True
buttonsEqualityTest _ _ = False

-- key press translation
eventToButton :: Event -> Maybe Button
eventToButton (KeyPress " ") = Just Stop
eventToButton (KeyPress "Up") = Just GoUp
eventToButton (KeyPress "Down") = Just GoDown
eventToButton _ = Nothing

-- The event handling function, which updates the state given an event.
nextModeByEvent :: Event -> Mode -> Mode
nextModeByEvent event st = applyAction action buttonsEqualityTest elevator st
  where 
    action = eventToButton event

-- Solution 2 
solution2 :: IO()
solution2 = activityOf Idle nextModeByEvent drawMode


-- 1.3 Interactive FSM

-- | Interactive finite state machine simulation.
interactiveFSM
  :: s                  -- ˆ Initial state.
  -> (a -> a -> Bool)   -- ˆ Action equality test.
  -> (s -> [(a, s)])    -- ˆ State transitions.
  -> (Event -> Maybe a) -- ˆ How to convert events into actions.
  -> (s -> Picture)     -- ˆ How to draw states.
  -> (a -> Picture)     -- ˆ How to draw actions.
  -> IO ()
interactiveFSM st equalityTest tr eventToButton' drawMode' drawButton'
  = activityOf st nextModeByEvent' draw
  where
    nextModeByEvent' event mode = applyAction (eventToButton' event) equalityTest tr mode
    draw mode = (drawMode' mode) <> (translated 3 0 $ asSpaced 3 drawButton' $ map fst $ tr mode)

-- Solution 3
solution3 :: IO()
solution3 = interactiveFSM Idle buttonsEqualityTest elevator eventToButton drawMode drawButton


-- Main
main :: IO ()
-- main = solution1 
-- main = solution2 
main = solution3
