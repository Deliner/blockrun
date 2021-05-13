module BlockRun where

import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Line
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random


data World = World {
    player :: Player,
    spike_iter :: [Spike],
    score :: Int,
    speed :: Int,
    state :: State
}

data Player = Player {
    speed:: Float,
    x :: Float,
    y :: Float
}

data State = State {
    isMainMenu :: Bool,
    isGameOver :: Bool
}

data Spike = Spike {
    x :: Float,
    y :: Float,
    num :: Int
}


runBlockRun :: IO()
runBlockRun = do
    g <- newStdGen
    play display bgColor fps (initWorld g) drawWorld handleWorld updateWorld
    where
    display = InWindow "BlockRun" (screenWidth, screenHeight) (screenWidth, screenHeight)
    bgColor = white  
    fps     = 60   


initWorld :: StdGen -> World
initWorld g = World
  {  player = Player {
    speed = 0,
    x = playerOffset,
    y = bottomOffset
  },
    spike_iter = initSpikes g,
    score = 0,
    speed = spikeSpeed,
    state = State {
        isMainMenu = True,
        isGameOver = False
    }
}

initSpikes :: StdGen -> [Spikes]
initSpikes g =  zipWith initSpike (randomRs spikeInterval g) (randomRs spikeCount g)

initSpike :: x -> n -> Spike
initSpike x num = Spike {
    x = x,
    y = bottomOffset,
    num  = num
}

drawWorld :: World -> Picture
    
drawPlayer :: Player -> Picture

drawSpikes :: [Spike] -> Picture 

drawSpike :: Spike -> Picture 

drawScore :: Int -> Picture

handleWorld :: Event -> World -> World

updateWorld :: Float -> World -> World

checkGameOver :: World ->  Bool 

checkCollisions :: Player -> [Spikes] -> Bool

checkCollision :: Player -> Spile -> Bool

getSpikeRange :: (Float, Float)
getSpikeRange = (-h, h)

spikeInterval :: (Float, Float)
objectInterval = (350,550)

spikeCount :: (Int, Int)
spikeCount = (1,3)


spikeWidth :: Float
spikeWidth = 20

spikeHeight :: Float
spikeHeight = 20

spikeSpeed :: Float
spikeSpeed = 100

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 400

playerSize :: Float
playerSize = spikeWidth

playerOffset :: Int
playerOffset = screenWidth / 2

bottomOffset :: Int
bottomOffset = screenHeight / 20

gravity:: Float
gravity = -60


jumpSpeed :: Float
jumpSpeed = 100