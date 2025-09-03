module Main where

import System.Environment (getArgs)
import Automata.Elementary (step, seed)
import Render.ASCII (renderRow)

-- | Read an Int from args at position i, or use a default.
readArgOr :: Int -> [String] -> Int -> Int
readArgOr def xs i = case drop i xs of
  (v:_) -> case reads v of
             [(n,"")] -> n
             _        -> def
  _     -> def

-- | Usage: CellularAutomata [rule] [width] [steps]
--   Example: CellularAutomata 30 79 40
main :: IO ()
main = do
  args <- getArgs
  let rule  = readArgOr 30 args 0
      width = readArgOr 79 args 1
      steps = readArgOr 40 args 2
      initState = seed width
      rows = take steps (iterate (step rule) initState)
  mapM_ (putStrLn . renderRow) rows