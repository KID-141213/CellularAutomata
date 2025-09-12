module Automata.GOL where

import Data.Array
import System.IO
import Control.Concurrent (threadDelay)

-- | Dimensions of the board: number of columns (width) and rows (height).
columns, rows :: Int
columns = 20
rows    = 10

-- | The board: an array where (x,y) is the cell position and Int is the cell state (0 = dead, 1 = alive).
type Board = Array (Int,Int) Int

-- | The bounds of the board: from (0,0) to (columns-1, rows-1).
boardBounds :: ((Int,Int),(Int,Int))
boardBounds = ((0,0),(columns-1, rows-1))

-- | First set all board positions to 0, then add the initial glider with 1.
initBoard :: Board
initBoard =
  let ((x0,y0),(x1,y1)) = boardBounds
      base = [ ((x,y), 0) | x <- [x0..x1], y <- [y0..y1] ]
      gliderCells = [ (1,0), (2,1), (0,2), (1,2), (2,2) ]
      glider = [ ((x,y),1) | (x,y) <- gliderCells ]
  in array boardBounds (base ++ glider)

{-|
Compute the next generation of the board.

* Only the inner cells are updated; borders remain dead (0);
* A live cell with fewer than 2 or more than 3 neighbors dies;
* A dead cell with exactly 3 neighbors becomes alive.
-}
nextGen :: Board -> Board
nextGen a =
  a // [ ((x,y), rule (x,y)) | x <- [x0+1 .. x1-1], y <- [y0+1 .. y1-1] ]  -- incremental updates
  where
    ( (x0,y0), (x1,y1) ) = bounds a
    neighbors (x,y) =
      a!(x-1, y-1) + a!(x, y-1) + a!(x+1, y-1) +
      a!(x-1, y  )              + a!(x+1, y  ) +
      a!(x-1, y+1) + a!(x, y+1) + a!(x+1, y+1)
    rule (x,y) =
      let currentCellState = a!(x,y)
          n = neighbors (x,y)
      in case currentCellState of
           1 | n < 2     -> 0  -- n means numbers of neighbors
             | n > 3     -> 0
             | otherwise -> 1
           0 | n == 3    -> 1
           _             -> currentCellState

-- | Display the board in the terminal, showing alive cells as ● and dead cells as ○.
display :: Board -> IO ()
display a = do
  let ((x0,y0),(x1,y1)) = bounds a
  sequence_ [ putStrLn [if a!(x,y) == 1 then '●' else '○' | x <- [x0..x1]] | y <- [y0..y1] ]

-- | Generate board states every second, with pause/resume using key press.
toStop :: Int -> Board -> Bool -> IO ()
toStop i board paused = do
  k <- hReady stdin
  newPaused <- if k
                 then do _ <- getChar
                         return (not paused)
                 else return paused
  if newPaused
    then do
      threadDelay 100000
      toStop i board newPaused
    else do
      putStrLn ("Generation " ++ show i ++ ":")
      display board
      threadDelay 1000000
      toStop (i+1) (nextGen board) newPaused

runGOL :: IO ()
runGOL = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  let gen0 = initBoard
  putStrLn "Generation 0:"
  display gen0
  toStop 1 (nextGen gen0) False