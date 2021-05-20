module DataTypes where

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