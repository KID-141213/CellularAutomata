module Automata.Seeds
  ( World
  , step
  , fromStrings
  , toList
  , render
  , renderBoxed
  , drawBoxedIO
  , aliveChar
  , deadChar
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

type Cell  = (Int, Int)
type World = S.Set Cell

aliveChar, deadChar :: Char
aliveChar = '●'  -- alive = black
deadChar  = '○'  -- dead  = white

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
      isAlive ch = ch == aliveChar       
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
render w h world = renderBoxed (0, w-1, 0, h-1) world

renderBoxed :: (Int,Int,Int,Int) -> World -> [String]
renderBoxed (minX,maxX,minY,maxY) world =
  [ [ if (x,y) `S.member` world then aliveChar else deadChar
    | x <- [minX..maxX] ]
  | y <- [minY..maxY]
  ]

drawBoxedIO :: (Int,Int,Int,Int) -> World -> IO ()
drawBoxedIO box world = do
  mapM_ putStrLn (renderBoxed box world)
  hFlush stdout

