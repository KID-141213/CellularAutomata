module Automata.Seeds
  ( World
  , step
  , fromStrings
  , toList
  , render
  , renderBox
  , drawBox
  , alive
  , dead
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

type Cell  = (Int, Int)
type World = S.Set Cell

alive, dead :: Char
alive = '●'  
dead  = '○'  

toList :: World -> [Cell]
toList = S.toList

nbrs :: Cell -> [Cell]
nbrs (x,y) =
  [ (x-1,y-1),(x,y-1),(x+1,y-1)
  , (x-1,y  )         ,(x+1,y  )
  , (x-1,y+1),(x,y+1),(x+1,y+1)
  ]

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

step :: World -> World
step w =
  let counts = M.fromListWith (+)
             [ (c, 1 :: Int)
             | cell <- S.toList w
             , c    <- nbrs cell
             ]
      births = [ c | (c,k) <- M.toList counts, k == 2 ]
  in (S.fromList births) `S.difference` w  

render :: Int -> Int -> World -> [String]
render w h world = renderBox (0, w-1, 0, h-1) world

renderBox :: (Int,Int,Int,Int) -> World -> [String]
renderBox (minX,maxX,minY,maxY) world =
  [ [ if (x,y) `S.member` world then alive else dead
    | x <- [minX..maxX] ]
  | y <- [minY..maxY]
  ]

drawBox :: (Int,Int,Int,Int) -> World -> IO ()
drawBox box world = do
  mapM_ putStrLn (renderBox box world)
  hFlush stdout

