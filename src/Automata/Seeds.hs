{-|
Module      : Automata.Seeds
Description : Core logic and simple rendering for the Seeds CA (rule B2/S).

Seeds rule (B2/S):
- A cell is alive in the next step if it has exactly 2 live neighbors.
- Otherwise it is dead (even if it was alive before).

Coordinates:
- Cells are (x,y). x increases to the right, y increases downward.
- Rendering prints rows from top (small y) to bottom (large y).
-}

module Automata.Seeds where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

-- | A cell coordinate (x,y).
type Cell  = (Int, Int)

-- | The world: a set of live cells.
type World = S.Set Cell

-- | Characters used for rendering: live and dead.
alive, dead :: Char
alive = '●'
dead  = '○'

-- | Convert the world to a list of live cell coordinates.
toList :: World -> [Cell]
toList = S.toList

-- | 8-neighborhood (Moore) of a cell.
nbrs :: Cell -> [Cell]
nbrs (x,y) =
  [ (x-1,y-1),(x,y-1),(x+1,y-1)
  , (x-1,y  )         ,(x+1,y  )
  , (x-1,y+1),(x,y+1),(x+1,y+1)
  ]

-- | Build a world from lines of characters.
--   Cells equal to 'alive' are live; all others are dead.
--   The first string is the top row; leftmost char has x=0.
fromStrings :: [String] -> World
fromStrings rows =
  let ys = zip [0..] rows
      isAlive ch = ch == alive
      live =
        [ (x,y)
        | (y,row) <- ys
        , (x,ch)  <- zip [0..] row
        , isAlive ch
        ]
  in S.fromList live

-- | One step of the Seeds rule (B2/S).
--   Next generation = all cells with exactly 2 live neighbors,
--   minus the current live cells (because Seeds does not keep them alive).
step :: World -> World
step w =
  let counts = M.fromListWith (+)
             [ (c, 1 :: Int)
             | cell <- S.toList w
             , c    <- nbrs cell
             ]
      births = [ c | (c,k) <- M.toList counts, k == 2 ]
  in (S.fromList births) `S.difference` w

-- | Render a box from (0,0) to (w-1,h-1) as lines of text.
render :: Int -> Int -> World -> [String]
render w h world = renderBox (0, w-1, 0, h-1) world

-- | Render a given box (minX,maxX,minY,maxY) as lines of text (top to bottom).
--   Uses 'alive' for live cells and 'dead' for others.
renderBox :: (Int,Int,Int,Int) -> World -> [String]
renderBox (minX,maxX,minY,maxY) world =
  [ [ if (x,y) `S.member` world then alive else dead
    | x <- [minX..maxX] ]
  | y <- [minY..maxY]
  ]

-- | Print the rendered box to stdout and flush.
drawBox :: (Int,Int,Int,Int) -> World -> IO ()
drawBox box world = do
  mapM_ putStrLn (renderBox box world)
  hFlush stdout