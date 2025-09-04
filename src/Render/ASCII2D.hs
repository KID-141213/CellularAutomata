module Render.ASCII2D
  ( drawBoxed
  , clearScreen
  ) where

import qualified Data.Set as S
import System.IO (hFlush, stdout)

type Cell = (Int,Int)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H" >> hFlush stdout

drawBoxed :: (Int,Int,Int,Int) -> S.Set Cell -> IO ()
drawBoxed (minX,maxX,minY,maxY) world = do
  let alive = '○'
      dead  = '●'
      row y = [ if (x,y) `S.member` world then alive else dead
               | x <- [minX..maxX] ]
  mapM_ putStrLn [ row y | y <- [minY..maxY] ]
  hFlush stdout
