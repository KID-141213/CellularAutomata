module Main where

import Control.Concurrent (threadDelay)
import Automata.Seeds
import Render.ASCII2D

-- 5×5 初始图案（'#' 表示活；只是初始化用，输出会用黑白圈）
initial :: World
initial = fromStrings
  [ "●●●●●●●●●●"
  , "●●●●○○●●●●"
  , "●●○●○●○●●●"
  , "●●●●○○●●●●"
  , "●●●●●●●●●●"
  , "●●●●●●●●●●"
  , "●●●○○○○●●●"
  , "●●●●○●○●●●"
  , "●●●●○○●●●●"
  , "●●●●●●●●●●"
  ]

-- 显示窗口改成 10×10
box :: (Int,Int,Int,Int)
box = (0, 9, 0, 9)

loop :: Int -> World -> IO ()
loop 0 _ = pure ()
loop n w = do
  -- 不需要很快，这里留个小延时
  drawBoxed box w
  putStrLn ""               -- 每代空一行
  threadDelay 120000
  loop (n-1) (step w)

main :: IO ()
main = loop 20 initial       -- 只跑 10 次