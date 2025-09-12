module Main where

import Control.Concurrent (threadDelay)
import Automata.Seeds
import System.IO (hFlush, stdout, withFile, IOMode(WriteMode), hSetEncoding, hPutStrLn, utf8)

-- =========================
-- Initial configuration
-- =========================

-- 10x10 initial pattern (each row 10 cells)
initial :: World
initial = fromStrings
  [ "○○○○○○○○○○"
  , "○○○○●●○○○○"
  , "○○●○●○●○○○"
  , "○○○○●●○○○○"
  , "○○○○○○○○○○"
  , "○○○○○○○○○○"
  , "○○○○●●●●○○"
  , "○○○○●○●○○○"
  , "○○○○●●○○○○"
  , "○○○○○○○○○○"
  ]

-- fixed 10x10 box
box :: (Int,Int,Int,Int)
box = (0, 9, 0, 9)


loop :: Int -> World -> IO World
loop 0 w = pure w
loop n w = do
  let w' = step w
  drawBoxedIO box w'
  putStrLn ""
  threadDelay 120000
  loop (n-1) w'

-- =========================
-- IO helpers
-- =========================

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

-- Save with generation header on first line
saveWorldWithGen :: FilePath -> Int -> (Int,Int,Int,Int) -> World -> IO ()
saveWorldWithGen fp gen b w =
  withFile fp WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStrLn h ("# generation: " ++ show gen)
    mapM_ (hPutStrLn h) (renderBoxed b w)

-- =========================
-- REPL
-- =========================

helpText :: String
helpText = unlines
  [ "Commands:"
  , "  help               - show this help"
  , "  show               - draw current world once"
  , "  run N              - run N generations (with small delay)"
  , "  save [file]        - save world to file (default ca_world.txt) with generation header"
  , "  exit               - quit"
  ]

main :: IO ()
main = do
  putStrLn "Cellular Automaton — interactive control (fixed-box save, UTF-8, generation header)"
  putStrLn "Black circle ● = alive, White circle ○ = dead"
  putStrLn "Type 'help' to get the list of commands."
  repl initial 0

repl :: World -> Int -> IO ()
repl world gen = do
  line <- prompt ("> (" ++ show gen ++ ") ")
  let ws = words line
  case ws of
    [] -> repl world gen
    ("help":_) -> putStrLn helpText >> repl world gen
    ("show":_) -> drawBoxedIO box world >> repl world gen
    ("run":nstr:_) ->
      case reads nstr of
        [(n,"")] | n > 0 -> do
          putStrLn $ "Running " ++ show n ++ " generations..."
          w' <- loop n world
          repl w' (gen + n)
        _ -> putStrLn "run N : N must be integer>0" >> repl world gen
    ("save":rest) -> do
      let fn = case rest of { (f:_) -> f; _ -> "ca_world.txt" }
      saveWorldWithGen fn gen box world
      putStrLn $ "Saved generation " ++ show gen ++ " to " ++ fn
      repl world gen
    ("exit":_) -> putStrLn "Bye."
    _ -> putStrLn ("Unknown command: " ++ unwords ws ++ " (type help)") >> repl world gen




