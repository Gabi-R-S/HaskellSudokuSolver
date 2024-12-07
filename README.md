# HaskellSudokuSolver
Haskell functions to solve Sudoku puzzles.
## Algorithm used:
1) Calculate possible values for each position
2) Pick square with the lowest number of possible alternative values and test each possibility
3) If the board in a branch is invalid, stop.
4) If all squares have a single possible value, a valid solution is added to the resulting list.
5) Repeat until all solutions have been found.
## Test:
In gchi, load the file, activate the array package and run the command _printSolutions (solve (listToBoard testBoard1))_.
