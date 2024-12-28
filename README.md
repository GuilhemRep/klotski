# Klotski
A tool to slove any sliding puzzle
![illustration](img.png) 

## How to use

Install OCaml (good luck) and LaTeX

- Compile the program using **make**
- Write the start and end configuration in *start.txt* and *end.txt*
- Run **make run** to obtain a pdf file containig the shortest solution if there is one, or an arror if not

For instance, write

*AABB*

*AVVB*

*CVVD*

*CCDD*

*VXXV*

*VXXV*

in *start.txt *and

*AABB*

*AXXB*

*CXXD*

*CCDD*

*VVVV*

*VVVV*

in *end.txt* for an easy variant of klotski.

# Special letters
The letter **V** is reserved for empty slots ("void"), and **O**     for obstacles that cannot move.

# Modes 
There are two modes: classical and labyrinth. The first, obtained with **make run** is for puzzles in which the first and last configuration are imposed. The second, obained with **make lab** is for puzzles in which only the piece labeled with **X** has a specified target position.