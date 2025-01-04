# Klotski
A tool to slove any sliding puzzle
![illustration](img.png) 

## Usage
- Install OCaml and LaTeX
- Compile the program using **make**
- Write the start and end configuration in *start.txt* and *end.txt*
- Run **make run** or **make lab** to obtain a *pdf* file containig the shortest solution if there is one, or an arror if not

## Designinig a puzzle
A puzzle piece is defined by a letter. The current example corresponds to the above picture.

## Special letters
To design a puzzle, keep in mind that an empty space and underscores **_** are reserved for empty slots, and **.** for obstacles that cannot move. All other letters and characters can be used to define pieces.

## Modes 
There are two modes: classical and labyrinth. The first, obtained with **make run** is for puzzles in which the first and last configuration are imposed. The second, obained with **make lab** is for puzzles in which only the piece labeled with **X** has a specified target position.