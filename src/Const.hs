module Const where


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