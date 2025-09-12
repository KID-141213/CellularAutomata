module Automata.GOL where

import Data.Array
import System.IO
import Control.Concurrent (threadDelay)

columns, rows :: Int
columns = 20
rows    = 10

type Board = Array (Int,Int) Int

boardBounds :: ((Int,Int),(Int,Int))
boardBounds = ((0,0),(columns-1, rows-1))

initBoard :: Board
initBoard =
  let ((x0,y0),(x1,y1)) = boardBounds
      base = [ ((x,y), 0) | x <- [x0..x1], y <- [y0..y1] ]
      gliderCells = [ (1,0), (2,1), (0,2), (1,2), (2,2) ]
      glider = [ ((x,y),1) | (x,y) <- gliderCells ]
  in array boardBounds (base ++ glider)

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

display :: Board -> IO ()
display a = do
  let ((x0,y0),(x1,y1)) = bounds a
  sequence_ [ putStrLn [if a!(x,y) == 1 then '●' else '○' | x <- [x0..x1]] | y <- [y0..y1] ]

stop :: Int -> Board -> Bool -> IO ()
stop i board paused = do
  k <- hReady stdin
  newPaused <- if k
                 then do _ <- getChar
                         return (not paused)
                 else return paused
  if newPaused
    then do
      threadDelay 100000
      stop i board newPaused
    else do
      putStrLn ("Generation " ++ show i ++ ":")
      display board
      threadDelay 1000000
      stop (i+1) (nextGen board) newPaused

runGOL :: IO ()
runGOL = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  let gen0 = initBoard
  putStrLn "Generation 0:"
  display gen0
  stop 1 (nextGen gen0) False