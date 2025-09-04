module Automata.Seeds
  ( World
  , step
  , fromStrings
  , toList
  , toSet
  , render         -- 固定从 (0,0) 开始、给定宽高的纯渲染
  , renderBoxed    -- 指定边界 (minX,maxX,minY,maxY) 的纯渲染
  , drawBoxedIO    -- 直接在终端打印（方便 Main 里直接用）
  , aliveChar
  , deadChar
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

-- | 坐标与世界
type Cell  = (Int, Int)
type World = S.Set Cell

-- 想要的字符：白圈=活，黑圈=死（两者大小基本一致）。
aliveChar, deadChar :: Char
aliveChar = '○'  -- U+25CB WHITE CIRCLE
deadChar  = '●'  -- U+25CF BLACK CIRCLE

toList :: World -> [Cell]
toList = S.toList

toSet :: World -> S.Set Cell
toSet = id

-- | Moore 邻域（8 邻域）
nbrs :: Cell -> [Cell]
nbrs (x,y) =
  [ (x-1,y-1),(x,y-1),(x+1,y-1)
  , (x-1,y  )         ,(x+1,y  )
  , (x-1,y+1),(x,y+1),(x+1,y+1)
  ]

-- | 从多行字符串初始化：
--   默认把 '○' 或 '#' 当作“活”，其他字符都视作“死”
fromStrings :: [String] -> World
fromStrings rows =
  let ys = zip [0..] rows
      isAlive ch = ch == aliveChar || ch == '#'
      live =
        [ (x,y)
        | (y,row) <- ys
        , (x,ch)  <- zip [0..] row
        , isAlive ch
        ]
  in S.fromList live

-- | Seeds (B2/S∅)：
--   - 所有活细胞下一代必死
--   - 恰好有 2 个活邻居的“死细胞”在下一代诞生
step :: World -> World
step w =
  let counts :: M.Map Cell Int
      counts = M.fromListWith (+)
             [ (c, 1 :: Int)
             | cell <- S.toList w
             , c    <- nbrs cell
             ]
      births = [ c | (c,k) <- M.toList counts, k == 2 ]
  in S.fromList births `S.difference` w  -- 阻止“原地重生”

-- | 纯渲染（从 (0,0) 开始，给定宽高）
render :: Int -> Int -> World -> [String]
render w h world =
  [ [ if (x,y) `S.member` world then aliveChar else deadChar
    | x <- [0..w-1] ]
  | y <- [0..h-1]
  ]

-- | 纯渲染（指定边界）
renderBoxed :: (Int,Int,Int,Int) -> World -> [String]
renderBoxed (minX,maxX,minY,maxY) world =
  [ [ if (x,y) `S.member` world then aliveChar else deadChar
    | x <- [minX..maxX] ]
  | y <- [minY..maxY]
  ]

-- | 直接在终端打印（方便在 Main 里调用）
drawBoxedIO :: (Int,Int,Int,Int) -> World -> IO ()
drawBoxedIO box world = do
  mapM_ putStrLn (renderBoxed box world)
  hFlush stdout
