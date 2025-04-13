# Puzzle-Solver

## Overview

This project implements a Prolog-based solver for a logical puzzle game. The program uses advanced Prolog techniques, including constraints and recursion, to validate and solve puzzles by placing objects on a grid while respecting specific rules. This project was a significant step in learning declarative programming and problem-solving with Prolog.

## Features

- **Grid Validation**: Ensures the puzzle grid is valid and adheres to the required dimensions.
- **Object Placement**: Places objects (e.g., tents, grass) on the grid based on puzzle rules.
- **Neighborhood Analysis**: Calculates adjacent and diagonal cells for logical operations.
- **Puzzle Solving**: Implements strategies to solve the puzzle, including filling inaccessible cells and ensuring valid object placements.

## How to Run

### Prerequisites

- **SWI-Prolog**: Install SWI-Prolog to run the program. You can install it using:
  ```bash
  sudo apt install swi-prolog
  ```
### Running the Program
1. Open the Prolog interpreter:
   `swipl`
2. Load the main file:
   `['Projeto.pl'].`
3. Define or load a puzzle to solve.
4. Solve the puzzle by calling the appropriate predicate (e.g., `solvePuzzle/1`).

#### Example Puzzle
Here is an example puzzle to solve:
```
% Puzzle definition
puzzle([
    [_, _, _, _],
    [_, a, _, _],
    [_, _, _, _],
    [_, _, _, a]
], [1, 1, 1, 1], [1, 1, 1, 1]).

% Solve the puzzle
?- puzzle(Tabuleiro, Linhas, Colunas),
   solvePuzzle((Tabuleiro, Linhas, Colunas)).
```
#### Example Usage
To validate a puzzle grid:
```
?- verificaT([[_,_,_],[_,_,_],[_,_,_]]).
true.
```
To solve the example puzzle:
```
?- puzzle(Tabuleiro, Linhas, Colunas),
   solvePuzzle((Tabuleiro, Linhas, Colunas)).
```
Expected output:
```
Tabuleiro = [
    [r, t, r, r],
    [r, a, r, r],
    [r, r, r, t],
    [r, r, r, a]
].
```
## Key Components
Validation
- `verificaT/1`: Validates the puzzle grid dimensions.
- `valida/2`: Ensures each tree has exactly one tent connected to it.

Object Placement
- `insereObjectoCelula/3`: Places an object (e.g., tent, grass) in a specific cell.
- `insereObjectoEntrePosicoes/4`: Places objects in a range of cells.

Puzzle Solving
- `relva/1`: Fills rows and columns with grass where the required number of tents is already placed.
- `inacessiveis/1`: Marks inaccessible cells with grass.
- `unicaHipotese/1`: Places a tent in cells with exactly one valid option.

Neighborhood Analysis
- `vizinhanca/2`: Calculates adjacent cells for a given position.
- `vizinhancaAlargada/2`: Calculates both adjacent and diagonal cells.

## Author
Developed by Guilherme Monteiro. For more information, visit [my GitHub profile](https://github.com/Monteir016).
