module Main where

import           Graphics.Gloss
import           System.Random
import System.Environment
import           Percolation
import           Draw

color1 = greyN 0.4
color2 = greyN 0.6
backgroundColor = greyN 0.1
pathColor = rose
strokeWidth = 0.25
scaleFactor = 5

genPicture :: Int -> IO Picture
genPicture size = do
  seed <- getStdGen
  let grid = createGrid seed size
      grid_coloured =
        map (\(coord, f) -> (coord, if f then color1 else color2)) grid
      boundary = percolationBoundary grid
  return $ scale scaleFactor scaleFactor $
           translate (- sqrt 3 * 0.5 * fromIntegral size) (-0.75 * fromIntegral size) $
           pictures [drawGrid grid_coloured, drawPath pathColor strokeWidth boundary]

main = do
  args <- getArgs
  let size = case args of
        (x : _) -> read x
        _ -> 50

  picture <- genPicture size
  display (InWindow
           "SLE" -- window title
           (ceiling $ scaleFactor * sqrt 3 * (fromIntegral size + 3), ceiling $ scaleFactor * 1.5 * (fromIntegral size + 3)) -- window size
           (0, 0)) -- window position
           backgroundColor -- background color
           picture
