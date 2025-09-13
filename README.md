# Cellular Automata
## Authors
- The implementation of **Game of Life** and an interactive mechanism to start/pause simulations — *Mengtong Chen*
- The implementation of **Seeds** and mechanisms to store and load a CA — *Haogang Zhang*
- Integration of the two projects — *Haogang Zhang*
- The cabal build was done with the assistance of *ChatGPT*

## Game of Life in Haskell
This project was inspired by the **array-based** implementation idea from a Youtuber *@The Coding Train*.
### Features
- Board initialized with a **glider**;
- The CA is displayed in the terminal using ASCII characters;
- The board updates automatically every second to visualize the evolution;
- The game supports pause/resume by pressing `Enter`;
- Press `Ctrl+C` to quit the program.

## Seeds in Haskell
Seeds rule: every live cell dies; a dead cell with exactly two live neighbors (Moore neighborhood) becomes alive.
### Features
- Includes a 10×10 demo pattern (can be modified in the code);
- Displays the cellular automaton in the terminal using ASCII characters;
- Provides a TUI with the following commands: `help`, `show`, `run N`, `save [file]`, `exit`;
- Saves the current world as UTF-8 text with a `# generation: N` header.
- The cellular automaton logic is in seed.hs, while the program entry point and features  are in Main.hs.

## Haddock
Haddock documentation is generated at:
`CellularAutomata\dist-newstyle\build\x86_64-windows\ghc-9.8.4\CellularAutomata-0.1.0.0\doc\html\CellularAutomata\`