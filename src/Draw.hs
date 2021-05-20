module Draw where

import           Graphics.Gloss
import System.Random
import DataTypes
import Const


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