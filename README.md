# Klotski
_Tool to slove sliding puzzles_
![illustration](img.png) 

## Usage
- Install OCaml and LaTeX
- Compile the OCaml part using **make**, or **dune build**
- Define the start and end configuration of the puzzle in *start.txt* and *end.txt*
- Run **make mode** with one of the _modes_ described below to obtain a *pdf* file containing a description of an eventual shortest solution, or an error if there is none.

## Designing a puzzle
A puzzle piece is defined by a letter. The current example corresponds to the above picture. The size of the puzzle is arbitrary. The text files must end with an empty line.

## Special characters
To design a puzzle, use underscores **_** to represent empty slots, and **.** for obstacles that cannot move. All other letters can be used to define pieces. For instance, here is a representation of the classical starting position :

```
AXXB
AXXB
_CC_
DFGE
DHIE

```

## Modes 
There are three modes: full, labyrinth and tangram.
* The first, obtained with **make full** is for puzzles in which the first and last configuration are imposed.
* The second, obtained with **make lab** is when only the piece labeled with **X** has a specified target position.
* The third, obtained with **make tangram** tries to slide the pieces described in *start.txt* into all non-empty slots of *end.txt*. This mode is used for puzzles that focus on transforming shapes.

## GUI (new!)
Use **make gui** to design and solve a puzzle in graphical mode. (still unstable)

### Tests
You can run multiple tests with **make test** or **dune runtest**, in particular to check that the algorithm finds a shortest solution for the examples provided in the example folders. 