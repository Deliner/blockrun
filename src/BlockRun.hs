module BlockRun where

import           Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import           System.Random
import Const
import DataTypes
import Init
import Draw
import Collision


runBlockRun :: IO()
runBlockRun = do
    g <- newStdGen
    play display bgColor fps (initWorld g) drawWorld handleWorld updateWorld
    where
    display = InWindow "BlockRun" (round screenWidth, round screenHeight) (100, 100)
    bgColor = white
    fps = 60


handleWorld :: Event -> World -> World
handleWorld event world
  | isGameOver (state world) = case event of
    EventKey (SpecialKey KeyEnter) Up _ _ -> resetWorld world
    _ -> world
  | isMainMenu (state world) = case event of
    EventKey (SpecialKey KeyEnter) Up _ _ -> world {state = (state world) {isMainMenu = False}}
    _ -> world
  | otherwise = case event of
    (EventKey (SpecialKey KeySpace) Down _ _) ->
      if playerY (player world) > bottomOffset
        then world
        else world {player = (player world) { playerSpeed = jumpSpeed}}
    _ -> world

updateWorld :: Float -> World -> World
updateWorld dt world
  | isMainMenu (state world) = world
  | checkGameOver world = world {state = (state world) {isGameOver = True}}
  | otherwise =
    world
      { player =
          (player world)
            { playerY = movePlayer (player world) dt,
              playerSpeed = if playerSpeed (player world) < -jumpSpeed then 0 else playerSpeed (player world) - gravity*dt
            },
        worldSpeed = spikeSpeed + 5 * score world,
        spikeIter = moveSpikes (worldSpeed world) dt (spikeIter world),
        score = score world + dt
      }


movePlayer :: Player -> Float -> Float
movePlayer player dt
  | playerBelow player = bottomOffset
  | otherwise = playerY player + dt * playerSpeed player
    where
      playerBelow :: Player -> Bool
      playerBelow player = playerY player + dt * playerSpeed player < bottomOffset

moveSpikes :: Float -> Float -> [Spike] -> [Spike]
moveSpikes _ _ [] = []
moveSpikes speed dt (spikeH : spikeT)
  | dx > pos = spikeT
  | otherwise = spikeH {spikeX = spikeX spikeH - dx} : spikeT
  where
    pos = spikeX spikeH + spikeRange + 3 * spikeWidth
    dx = dt * speed

checkGameOver :: World ->  Bool
checkGameOver world = checkCollisions (player world) (spikeIter world)