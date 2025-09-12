# Cellular Automata
## Authors
- The implementation of **Game of Life** and an interactive mechanism to start/pause simulations — *Mengtong Chen*
- The implementation of **Seeds** and mechanisms to store and load a CA — *Haogang Zhang*
- Integration of the two projects — *Haogang Zhang*

## Game of Life in Haskell
This project was inspired by the **array-based** implementation idea from a Youtuber *@The Coding Train*, and the cabal build was done with the assistance of *ChatGPT*.
### Features
- Board initialized with a **glider**;
- The CA is displayed in the terminal using ASCII characters;
- The board updates automatically every second to visualize the evolution;
- The game supports pause/resume by pressing `Enter`;
- Press `Ctrl+C` to quit the program.

## Seeds in Haskell

A minimal terminal simulator for the 2D **Seeds** cellular automaton.

### Features
- Ships with a **10×10** demo pattern (editable in code);
- Unicode rendering in the terminal: **● = alive**, **○ = dead**;
- Interactive REPL with `help`, `show`, `run N`, `save [file]`, `exit`;
- Saves the current world as UTF-8 text with a `# generation: N` header;
- Seeds rule: every live cell dies; a dead cell with **exactly two** live neighbors (Moore neighborhood) becomes alive;