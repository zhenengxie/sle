module Main where

import Graphics.Gloss
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal
import System.Random
import qualified Data.Map.Strict as M

type Coord = (Int, Int)
type Side = Int
data Edge = Edge Coord Side deriving Eq

createGrid :: RandomGen g => g -> Int -> [(Coord, Color)]
createGrid seed size = map correctCell $ zip cells values
  where
    cells = do
      i <- [0..size]
      j <- [0..i]
      return (i - j, j)
    values = map (\x -> if x then color1 else color2) $ randoms seed
    correctCell ((i, j), val)
      | j == 0 = ((i, j), color1)
      | i == 0 = ((i, j), color2)
      | otherwise = ((i, j), val)

percolationBoundary :: [(Coord, Color)] -> [Edge]
percolationBoundary gridList = go $ Edge (0, 0) 2
  where
    grid = M.fromList gridList
    go edge@(Edge cell side) = case M.lookup (shift ((side + 1) `mod` 6) cell) grid of
      (Just _) -> edge : go (nextEdge grid color1 edge)
      _ -> [edge]

nextEdge :: Eq a => M.Map Coord a -> a -> Edge -> Edge
nextEdge grid val0 (Edge cell side) = let
  testSide = (side + 1) `mod` 6
  testCell = shift testSide cell
  in case M.lookup testCell grid of
       Just val -> case val == val0 of
         True -> Edge testCell $ (side + 5) `mod` 6
         False -> Edge cell testSide
       _ -> Edge cell testSide

shift :: Side -> Coord -> Coord
shift side (x, y) = case side of
  0 -> (x - 1, y)
  1 -> (x -1, y + 1)
  2 -> (x, y + 1)
  3 -> (x + 1, y)
  4 -> (x + 1, y - 1)
  5 -> (x, y - 1)
  x -> error (show x ++ " index out of bounds")

color1 = greyN 0.4
color2 = greyN 0.6
backgroundColor = greyN 0.1
pathColor = green
strokeWidth = 0.15


hexagon :: Picture
hexagon = polygon [(0, 1), (sqrt 3 / 2, 0.5), (sqrt 3 / 2, -0.5), (0, -1), (-sqrt 3 / 2, -0.5), (-sqrt 3 / 2, 0.5)]

drawPath :: [Edge] -> Picture
drawPath p = pictures $ map drawEdge p

drawEdge :: Edge -> Picture
drawEdge (Edge coord side) = translateHex coord $
                         rotate (fromIntegral side * 60) $
                         color pathColor edge
  where x = -sqrt 3 / 2
        y = 0.5
        edge = pictures [ translate x (- y) $ circleSolid strokeWidth
                        , translate x y $ circleSolid strokeWidth
                        , polygon [ (x - strokeWidth, y)
                                  , (x + strokeWidth, y)
                                  , (x + strokeWidth, -y)
                                  , (x -strokeWidth, -y)]
                        ]

drawGrid :: [(Coord, Color)] -> Picture
drawGrid grid = pictures $ map drawCell grid

drawCell :: (Coord, Color) -> Picture
drawCell (coord, cellColor) = Color cellColor $ translateHex coord cell
  where cell = scale 0.8 0.8 hexagon

translateHex :: Coord -> Picture -> Picture
translateHex (x, y) = translate x_trans y_trans
  where
    x_num = fromIntegral x
    y_num = fromIntegral y
    x_trans = sqrt 3 * (x_num + 0.5 * y_num)
    y_trans = 1.5 * y_num

getPicture :: IO Picture
getPicture = do
  seed <- getStdGen
  let grid = createGrid seed 250
      boundary = percolationBoundary grid
  return $ pictures [drawGrid grid, drawPath boundary]

main = do
  picture <- getPicture
  display
       (InWindow
         "SLE" -- window title
         (400, 400) -- window size
         (0, 0) -- window position
       )
       backgroundColor -- background color
       picture
