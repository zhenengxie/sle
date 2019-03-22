module Percolation where

import           Math.Geometry.Grid.Hexagonal
import           Math.Geometry.Grid
import           System.Random
import qualified Data.Map.Strict               as M

type Coord = (Int, Int)
type Side = Int
data Edge = Edge Coord Side deriving Eq

percolationBoundary :: Coord -> Side -> [(Coord, Bool)] -> [Edge]
percolationBoundary initialCell initialSide gridList = go
  $ Edge initialCell initialSide
 where
  grid       = M.fromList gridList
  initialVal = case M.lookup initialCell grid of
    Just v -> v
    _      -> error "cell not in grid"
  go edge@(Edge cell side) =
    case M.lookup (shift ((side + 1) `mod` 6) cell) grid of
      (Just _) -> edge : go (nextEdge grid initialVal edge)
      _        -> [edge]

nextEdge :: Eq a => M.Map Coord a -> a -> Edge -> Edge
nextEdge grid val0 (Edge cell side) =
  let testSide = (side + 1) `mod` 6
      testCell = shift testSide cell
  in  case M.lookup testCell grid of
        Just val -> case val == val0 of
          True  -> Edge testCell $ (side + 5) `mod` 6
          False -> Edge cell testSide
        _ -> Edge cell testSide

shift :: Side -> Coord -> Coord
shift side (x, y) = case side of
  0 -> (x - 1, y)
  1 -> (x - 1, y + 1)
  2 -> (x, y + 1)
  3 -> (x + 1, y)
  4 -> (x + 1, y - 1)
  5 -> (x, y - 1)
  x -> error (show x ++ " index out of bounds")

createGridTri :: RandomGen g => g -> Int -> [(Coord, Bool)]
createGridTri seed size = map colorCell $ zip cells values
 where
  cells = do
    i <- [0 .. size]
    j <- [0 .. i]
    return (i - j, j)
  values = randoms seed
  colorCell ((i, j), val) | j == 0    = ((i, j), True)
                          | i == 0    = ((i, j), False)
                          | otherwise = ((i, j), val)

percolationBoundaryTri :: [(Coord, Bool)] -> [Edge]
percolationBoundaryTri = percolationBoundary (0, 0) 2

createGridHex :: RandomGen g => g -> Int -> [(Coord, Bool)]
createGridHex seed size = map colorCell $ zip cells values
 where
  cells  = indices $ hexHexGrid size
  values = randoms seed
  colorCell ((i, j), val) = ((i, j), newVal)
   where
    newVal | i + j == size - 1 = False -- UR
           | i + j == 1 - size = True  -- DL
           | i == 1 - size     = False -- UL
           | i == size - 1     = True  -- DR
           | j == size - 1     = False -- U
           | j == 1 - size     = True  -- D
           | otherwise         = val

percolationBoundaryHex :: Int -> [(Coord, Bool)] -> [Edge]
percolationBoundaryHex size = percolationBoundary (-size + 1, 0) 2
