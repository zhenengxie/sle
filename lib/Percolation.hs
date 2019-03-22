module Percolation where

import           System.Random
import qualified Data.Map.Strict               as M

type Coord = (Int, Int)
type Side = Int
data Edge = Edge Coord Side deriving Eq

createGrid :: RandomGen g => g -> Int -> [(Coord, Bool)]
createGrid seed size = map colorCell $ zip cells values
  where
    cells = do
        i <- [0 .. size]
        j <- [0 .. i]
        return (i - j, j)
    values = randoms seed
    colorCell ((i, j), val) | j == 0    = ((i, j), True)
                            | i == 0    = ((i, j), False)
                            | otherwise = ((i, j), val)

percolationBoundary :: [(Coord, Bool)] -> [Edge]
percolationBoundary gridList = go $ Edge (0, 0) 2
  where
    grid = M.fromList gridList
    go edge@(Edge cell side) =
        case M.lookup (shift ((side + 1) `mod` 6) cell) grid of
            (Just _) -> edge : go (nextEdge grid True edge)
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