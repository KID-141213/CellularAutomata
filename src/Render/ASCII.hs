module Render.ASCII
  ( renderRow
  ) where

-- | Render a row of cells as ASCII.
--   True -> '█', False -> ' '
renderRow :: [Bool] -> String
renderRow = map cellChar
  where
    cellChar True  = '█'
    cellChar False = ' '