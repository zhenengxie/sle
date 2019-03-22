module Main where

import           Graphics.Gloss
import           System.Random
import           System.Environment
import           Percolation
import           Draw

color1 = greyN 0.4
color2 = greyN 0.6
backgroundColor = greyN 0.1
pathColor = rose
strokeWidth = 0.3
scaleFactor = 5

genPictureTri :: Int -> IO Picture
genPictureTri size = do
  seed <- getStdGen
  let grid = createGridTri seed size
      grid_coloured =
        map (\(coord, f) -> (coord, if f then color1 else color2)) grid
      boundary = percolationBoundaryTri grid
  return
    $ scale scaleFactor scaleFactor
    $ translate (-sqrt 3 * 0.5 * fromIntegral size) (-0.75 * fromIntegral size)
    $ pictures [drawGrid grid_coloured, drawPath pathColor strokeWidth boundary]

genPictureHex :: Int -> IO Picture
genPictureHex size = do
  seed <- getStdGen
  let grid = createGridHex seed size
      grid_coloured =
        map (\(coord, f) -> (coord, if f then color1 else color2)) grid
      boundary = percolationBoundaryHex size grid
  return $ scale scaleFactor scaleFactor $ pictures
    [drawGrid grid_coloured, drawPath pathColor strokeWidth boundary]

main = do
  args <- getArgs
  let size = case args of
        (x : _) -> read x
        _       -> 50

  picture <- genPictureHex size
  display (InWindow "SLE" -- window title
                          (1000, 1000) -- window size
                                       (0, 0)) -- window position
                                               backgroundColor -- background color
                                                               picture
