module BlockRun where

import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Line
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random


data World = World {
    player :: Player,
    spikeIter :: [Spike],
    score :: Float,
    worldSpeed :: Float,
    state :: State
}

data Player = Player {
    playerSpeed:: Float,
    playerX :: Float,
    playerY :: Float
}

data State = State {
    isMainMenu :: Bool,
    isGameOver :: Bool
}

data Spike = Spike {
    spikeX :: Float,
    spikeY :: Float,
    spikeNum :: Int
}


runBlockRun :: IO()
runBlockRun = do
    g <- newStdGen
    play display bgColor fps (initWorld g) drawWorld handleWorld updateWorld
    where
    display = InWindow "BlockRun" (round screenWidth, round screenHeight) (100, 100)
    bgColor = white
    fps = 60


initWorld :: StdGen -> World
initWorld g = World
  {
    player = Player {
        playerSpeed = 0,
        playerX = playerXOffset,
        playerY = bottomOffset
    },
    spikeIter = initSpikes g,
    score = 0,
    worldSpeed = spikeSpeed,
    state = State {
        isMainMenu = True,
        isGameOver = False
    }
}

resetWorld :: World -> World
resetWorld world = World
  {
    player = Player {
        playerSpeed = 0,
        playerX = playerXOffset,
        playerY = bottomOffset
    },
    spikeIter = tail (spikeIter world),
    score = 0,
    worldSpeed = spikeSpeed,
    state = State {
        isMainMenu = True,
        isGameOver = False
    }
}

initSpikes :: StdGen -> [Spike]
initSpikes g =  zipWith initSpike (randomRs spikeInterval g) (randomRs spikeCount g)

initSpike :: Float -> Int -> Spike
initSpike x num = Spike {
    spikeX = x,
    spikeY = bottomOffset,
    spikeNum  = num
}

drawWorld :: World -> Picture
drawWorld world
    | isMainMenu (state world) =  Pictures
        [ Translate (- screenWidth /6) (screenHeight / 10) . Scale 0.6 0.6 . Text $ "BlockRun",
          Translate (- screenWidth /7) (-screenHeight / 10) . Scale 0.15 0.15 . Text $ "Press 'Enter' to start"
        ]
    | isGameOver (state world) = Pictures
        [ Translate (- screenWidth / 7) 0 . Color red . Scale 0.4 0.4 . Text $ "GAME OVER",
          Translate (- screenWidth / 8) (-screenHeight / 10) . Scale 0.2 0.2 . Text $ "Score: " ++ currentScore,
          Translate (- screenWidth / 8) (-screenHeight / 6) . Scale 0.1 0.1 . Text $ "Press 'Enter' to restart"
        ]
    | otherwise = Pictures
        [
          drawSpikes (spikeIter world),
          drawPlayer (player world),
          drawScore (score world)
        ]
    where
      currentScore = show . round $ score world

drawPlayer :: Player -> Picture
drawPlayer player =
  Translate
      (playerX player)
      (playerY player)
      ( Color black $ Polygon
          [ ( playerSize / 2, playerSize / 2)
          , (-playerSize / 2, playerSize / 2)
          , (-playerSize / 2, -playerSize / 2)
          , ( playerSize / 2, -playerSize / 2)
          ]
      )

absoluteSpikes :: [Spike] -> [Spike]
absoluteSpikes = go 0
  where
    go _ [] = []
    go s (spikeH : spikeT) = spikeH {spikeX = spikeX spikeH + s} : go ( s + spikeRange + spikeX spikeH ) spikeT

drawSpikes :: [Spike] -> Picture
drawSpikes = Pictures . map drawNSpikes . takeWhile onScreen . absoluteSpikes
  where
    onScreen spike = spikeX spike < screenWidth

drawNSpikes :: Spike -> Picture
drawNSpikes spike
  | spikeNum spike == 1 = drawSpike spike 0
  | spikeNum spike == 3 =
    Pictures
      [ drawSpike spike 0,
        drawSpike spike spikeWidth,
        drawSpike spike (spikeWidth * 2)
      ]
  | otherwise =
    Pictures
      [ drawSpike spike 0,
        drawSpike spike spikeWidth
      ]

drawSpike :: Spike -> Float -> Picture
drawSpike spike offset = Translate x y
    ( Color black $ Polygon
      [ (0.0, playerSize / 2)
      , (- playerSize / 2, - playerSize / 2)
      , (playerSize / 2, - playerSize / 2)
      ]
    )
    where
        x = spikeX spike + offset
        y = spikeY spike

drawScore :: Float -> Picture
drawScore score = Translate x y
    ( Pictures [scale 0.2 0.2 (Color black (text (show (round score))))] )
    where
        x = - (screenWidth / 2) + 10
        y = screenHeight / 2 - 25

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

checkCollisions :: Player -> [Spike] -> Bool
checkCollisions player spikes = and (zipWith checkCollision [(playerX player, playerY player)] spikes)

checkCollision ::  (Float, Float) -> Spike -> Bool
checkCollision (playerX, playerY) spike =
  playerX >= (spikeX spike - spikeWidth)
    && playerX <= (spikeX spike + fromIntegral (spikeNum spike) * spikeWidth)
    && playerY <= bottomOffset + spikeHeight


spikeInterval :: (Float, Float)
spikeInterval = (350,550)

spikeCount :: (Int, Int)
spikeCount = (1,3)

spikeWidth :: Float
spikeWidth = 20

spikeRange :: Float
spikeRange = screenWidth / 2

spikeHeight :: Float
spikeHeight = 20

spikeSpeed :: Float
spikeSpeed = 100

screenWidth :: Float
screenWidth = 800

screenHeight :: Float
screenHeight = 400

playerSize :: Float
playerSize = spikeWidth

playerXOffset :: Float
playerXOffset = 0

bottomOffset :: Float
bottomOffset = - (screenWidth / 4) + spikeHeight

gravity:: Float
gravity = jumpSpeed * 2

jumpSpeed :: Float
jumpSpeed = playerSize * 10