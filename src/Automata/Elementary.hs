module Automata.Elementary
  ( step
  , seed
  ) where

-- | Compute the next generation for a 1D elementary CA.
--   'rule' is an integer 0..255 (e.g., 30, 90, 110).
--   The state is a finite line of Bool (True = alive, False = dead).
step :: Int -> [Bool] -> [Bool]
step rule xs = [ next i | i <- [0..len-1] ]
  where
    len = length xs
    get j = if j < 0 || j >= len then False else xs !! j
    next i =
      let l = get (i-1)
          c = get i
          r = get (i+1)
          idx = patIndex l c r    -- 0..7
      in ruleBit rule idx

-- | Initial state: a single True in the center (classic demo).
seed :: Int -> [Bool]
seed n
  | n <= 0    = []
  | otherwise = [ i == mid | i <- [0..n-1] ]
  where
    mid = n `div` 2

-- Map a neighborhood (l,c,r) to a number 0..7 as per Wolfram code.
-- Pattern ordering: 111->7, 110->6, 101->5, 100->4, 011->3, 010->2, 001->1, 000->0
patIndex :: Bool -> Bool -> Bool -> Int
patIndex l c r = b l * 4 + b c * 2 + b r * 1
  where b True = 1
        b False = 0

-- Extract the bit at position 'idx' from 'rule' (0..7), where
-- idx=7 corresponds to pattern 111, idx=0 to 000.
ruleBit :: Int -> Int -> Bool
ruleBit rule idx =
  ((rule `div` (2 ^ idx)) `mod` 2) == 1