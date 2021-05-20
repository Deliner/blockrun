module Collision where

import DataTypes
import Const


checkCollisions :: Player -> [Spike] -> Bool
checkCollisions player spikes = and (zipWith checkCollision [(playerX player, playerY player)] spikes)

checkCollision ::  (Float, Float) -> Spike -> Bool
checkCollision (playerX, playerY) spike =
  playerX >= (spikeX spike - spikeWidth)
    && playerX <= (spikeX spike + fromIntegral (spikeNum spike) * spikeWidth)
    && playerY <= bottomOffset + spikeHeight