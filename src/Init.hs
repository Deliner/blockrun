module Init where

import DataTypes
import Const
import           System.Random
import Graphics.Gloss


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