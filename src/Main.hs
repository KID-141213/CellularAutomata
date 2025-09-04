module Main where

import Control.Concurrent (threadDelay)
import Automata.Seeds
import Render.ASCII2D

import System.IO (hFlush, stdout, withFile, IOMode(WriteMode), hSetEncoding, hPutStrLn, utf8)
import System.Directory (doesFileExist)
import Control.Exception (try, IOException)
import qualified Data.Set as S

-- =========================
-- 初始配置（你的原始内容保留）
-- =========================

-- 10x10 initial pattern (as you provided)
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

-- display box fixed to 10x10
box :: (Int,Int,Int,Int)
box = (0, 9, 0, 9)

-- 显示并推进：先 step 再画（保证 run N 会显示 N 次“更新后的”状态）
loop :: Int -> World -> IO ()
loop 0 _ = pure ()
loop n w = do
  let w' = step w            -- 先推进一代
  drawBoxed box w'           -- 然后显示推进后的状态
  putStrLn ""                -- 每代空一行
  threadDelay 120000
  loop (n-1) w'


-- =========================
-- Helper / IO utilities
-- =========================

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

worldToStringsBox :: (Int,Int,Int,Int) -> World -> [String]
worldToStringsBox (xmin,xmax,ymin,ymax) w =
  [ [ if S.member (x,y) w then aliveChar else deadChar
    | x <- [xmin..xmax] ]
  | y <- [ymin..ymax]
  ]



-- Write fixed-box world to file with UTF-8 encoding
saveWorldBox :: FilePath -> (Int,Int,Int,Int) -> World -> IO ()
saveWorldBox fp b w =
  withFile fp WriteMode $ \h -> do
    hSetEncoding h utf8
    mapM_ (hPutStrLn h) (worldToStringsBox b w)

-- Save with generation header: first line "# generation: N"
saveWorldWithGen :: FilePath -> Int -> (Int,Int,Int,Int) -> World -> IO ()
saveWorldWithGen fp gen b w =
  withFile fp WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStrLn h ("# generation: " ++ show gen)
    mapM_ (hPutStrLn h) (worldToStringsBox b w)

-- Load file and attempt to parse "# generation: N" header.
-- Returns Nothing on read error or missing file.
loadWorldMaybeGen :: FilePath -> IO (Maybe (World, Int))
loadWorldMaybeGen fp = do
  ok <- doesFileExist fp
  if not ok then return Nothing else do
    eres <- try (readFile fp) :: IO (Either IOException String)
    case eres of
      Left _ -> return Nothing
      Right s ->
        let ls = lines s in
        case ls of
          (h:rest) | Just g <- parseGenHeader h -> return $ Just (fromStrings rest, g)
          _ -> return $ Just (fromStrings ls, 0)

parseGenHeader :: String -> Maybe Int
parseGenHeader s =
  case words s of
    ("#":"generation:":n:_) -> case reads n of [(g,"")] -> Just g; _ -> Nothing
    _ -> Nothing

-- Dynamic (auto-cropped) conversion (kept for compatibility / other features)
worldToStrings :: World -> [String]
worldToStrings w
  | S.null w = []
  | otherwise =
      let cells = S.toList w
          xs = map fst cells
          ys = map snd cells
          xmin = minimum xs
          xmax = maximum xs
          ymin = minimum ys
          ymax = maximum ys
      in [ [ if S.member (x,y) w then aliveChar else deadChar
           | x <- [xmin..xmax] ]
         | y <- [ymax, ymax-1 .. ymin] ]

toggleCell :: World -> (Int,Int) -> World
toggleCell w coord = if S.member coord w then S.delete coord w else S.insert coord w

-- =========================
-- REPL & commands
-- =========================

helpText :: String
helpText = unlines
  [ "Commands:"
  , "  help               - show this help"
  , "  show               - draw current world once"
  , "  dump               - print exactly what will be saved (fixed box)"
  , "  gen                - show current generation number"
  , "  step               - advance one generation and display"
  , "  run N              - run N generations (with small delay)"
  , "  run-default        - run your original loop 20 initial"
  , "  toggle X Y         - toggle cell at integer coords X Y"
  , "  save [file]        - save world to file (default ca_world.txt) with generation header"
  , "  load [file]        - load world from file (default ca_world.txt) and restore generation"
  , "  reset              - reset to initial pattern (generation -> 0)"
  , "  exit               - quit"
  ]

main :: IO ()
main = do
  putStrLn "Cellular Automaton — 交互式控制（fixed-box save, UTF-8, generation header）"
  putStrLn "白圈 ○ = alive, 黑圈 ● = dead"
  putStrLn "输入 help 获取命令列表。"
  repl initial 0

repl :: World -> Int -> IO ()
repl world gen = do
  line <- prompt ("> (" ++ show gen ++ ") ")
  let ws = words line
  case ws of
    [] -> repl world gen

    ("help":_) -> putStrLn helpText >> repl world gen

    ("gen":_) -> putStrLn ("Current generation: " ++ show gen) >> repl world gen

    ("show":_) -> drawBoxed box world >> repl world gen

    ("dump":_) -> do
      putStrLn $ "----- Dump (will save box " ++ show box ++ ") -----"
      mapM_ putStrLn (worldToStringsBox box world)
      putStrLn "----- End dump -----"
      repl world gen

    ("step":_) -> do
      let w' = step world
      drawBoxed box w'
      repl w' (gen + 1)

    ("run-default":_) -> do
      putStrLn "Running original loop 20 initial..."
      loop 20 initial
      repl initial 20

    ("run":nstr:_) ->
      case reads nstr of
        [(n,"")] | n > 0 -> do
          putStrLn $ "Running " ++ show n ++ " generations..."
          loop n world
          let w' = iterate step world !! n
          repl w' (gen + n)
        _ -> putStrLn "run N : N must be integer>0" >> repl world gen

    ("toggle":xs) ->
      case xs of
        (sx:sy:_) ->
          case (reads sx, reads sy) of
            ([(x,"")], [(y,"")]) -> do
              let w' = toggleCell world (x,y)
              putStrLn $ "Toggled (" ++ show x ++ "," ++ show y ++ ")"
              drawBoxed box w'
              repl w' gen
            _ -> putStrLn "toggle X Y : X,Y must be integers" >> repl world gen
        _ -> putStrLn "toggle X Y" >> repl world gen

    ("save":rest) -> do
      let fn = case rest of { (f:_) -> f; _ -> "ca_world.txt" }
      saveWorldWithGen fn gen box world
      putStrLn $ "Saved generation " ++ show gen ++ " to " ++ fn
      repl world gen

    ("load":rest) -> do
      let fn = case rest of { (f:_) -> f; _ -> "ca_world.txt" }
      m <- loadWorldMaybeGen fn
      case m of
        Nothing -> putStrLn ("Failed to load " ++ fn) >> repl world gen
        Just (w', g') -> do
          putStrLn ("Loaded " ++ fn ++ " (generation " ++ show g' ++ ")")
          drawBoxed box w'
          repl w' g'

    ("reset":_) -> putStrLn "Reset." >> drawBoxed box initial >> repl initial 0

    ("exit":_) -> putStrLn "Bye."

    ("quit":_) -> putStrLn "Bye."

    _ -> putStrLn ("Unknown command: " ++ unwords ws ++ " (type help)") >> repl world gen


