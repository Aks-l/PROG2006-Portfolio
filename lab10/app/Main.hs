import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Define the world with the circle's position
data World = World { circlePos :: (Float, Float) }

-- Initial state
initialWorld :: World
initialWorld = World (0, 0)

-- Render the world by drawing a red circle at its position
render :: World -> Picture
render world = translate x y $ color red $ circleSolid 50
  where (x, y) = circlePos world
 
-- Handle input events to update the world state
handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeyUp) Down _ _)    world = moveCircle (0, 10) world
handleInput (EventKey (SpecialKey KeyDown) Down _ _)  world = moveCircle (0, -10) world
handleInput (EventKey (SpecialKey KeyLeft) Down _ _)  world = moveCircle (-10, 0) world
handleInput (EventKey (SpecialKey KeyRight) Down _ _) world = moveCircle (10, 0) world
handleInput _ world = world

moveCircle :: (Float, Float) -> World -> World
moveCircle (dx, dy) world = world { circlePos = (x + dx, y + dy) }
  where (x, y) = circlePos world

-- Update function (not used for automatic changes here)
updateWorld :: Float -> World -> World
updateWorld _ world = world

main :: IO ()
main = play
         (InWindow "Interactive Gloss Window" (800, 600) (100, 100))
         white      -- background color
         60         -- frames per second
         initialWorld
         render
         handleInput
         updateWorld
