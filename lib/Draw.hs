module Draw where

import           Graphics.Gloss
import           Percolation

strokeWidth = 0.15

hexagon :: Picture
hexagon = polygon
    [ (0          , 1)
    , (sqrt 3 / 2 , 0.5)
    , (sqrt 3 / 2 , -0.5)
    , (0          , -1)
    , (-sqrt 3 / 2, -0.5)
    , (-sqrt 3 / 2, 0.5)
    ]

drawPath :: Color -> [Edge] -> Picture
drawPath c p = color c $ pictures $ map drawEdge p

drawEdge :: Edge -> Picture
drawEdge (Edge coord side) =
    translateHex coord $ rotate (fromIntegral side * 60) $ edge
  where
    x    = -sqrt 3 / 2
    y    = 0.5
    edge = pictures
        [ translate x (-y) $ circleSolid strokeWidth
        , translate x y $ circleSolid strokeWidth
        , polygon
            [ (x - strokeWidth, y)
            , (x + strokeWidth, y)
            , (x + strokeWidth, -y)
            , (x - strokeWidth, -y)
            ]
        ]

drawGrid :: [(Coord, Color)] -> Picture
drawGrid grid = pictures $ map drawCell grid

drawCell :: (Coord, Color) -> Picture
drawCell (coord, cellColor) = Color cellColor $ translateHex coord cell
    where cell = scale 0.8 0.8 hexagon

translateHex :: Coord -> Picture -> Picture
translateHex (x, y) = translate x_trans y_trans
  where
    x_num   = fromIntegral x
    y_num   = fromIntegral y
    x_trans = sqrt 3 * (x_num + 0.5 * y_num)
    y_trans = 1.5 * y_num
