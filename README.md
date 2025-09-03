# CellularAutomata — Minimal Haskell Cellular Automata (Elementary CA)

## Quick start (Cabal)

```bash
cd CellularAutomata
cabal update
cabal run CellularAutomata         # defaults: rule 30, width 79, steps 40
cabal run CellularAutomata -- 110 120 60  # rule=110, width=120, steps=60
```
If you see a triangle-like pattern for rule 30, everything works.

## Structure
- `src/Automata/Elementary.hs` — CA logic (rule application, seed).
- `src/Render/ASCII.hs` — simple ASCII renderer.
- `src/Main.hs` — CLI: `CellularAutomata [rule] [width] [steps]`.

## Next steps
- Add a 2D CA (e.g., Game of Life) under `Automata/Life.hs`.
- Add an interactive or graphical renderer (gloss / threepenny / SDL2).
- Add one compulsory feature (save/load, pause, set cells, etc.).
- Generate Haddock docs: `cabal haddock --open`.