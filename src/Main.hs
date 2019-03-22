module Main where

import           Graphics.Gloss
import           System.Random
import           Percolation
import           Draw

color1 = greyN 0.4
color2 = greyN 0.6
backgroundColor = greyN 0.1
pathColor = green

getPicture :: IO Picture
getPicture = do
  seed <- getStdGen
  let grid = createGrid seed 250
      grid_coloured =
        map (\(coord, f) -> (coord, if f then color1 else color2)) grid
      boundary = percolationBoundary grid
  return $ pictures [drawGrid grid_coloured, drawPath pathColor boundary]

main = do
  picture <- getPicture
  display (InWindow
           "SLE" -- window title
           (400, 400) -- window size
           (0, 0)) -- window position
           backgroundColor -- background color
           picture