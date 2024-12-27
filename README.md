# Klotski
A tool to slove any sliding puzzle

## How to use

Install OCaml, LaTeX

- Compile the program using **make**
- Write the start and end configuration in start.txt and end.txt
- Run **make.run** to obtain a pdf file containig the shortest solution if there is one, or an arror if not

For instance, write

*AABB*

*AVVB*

*CVVD*

*CCDD*

*VXXV*

*VXXV*

and

*AABB*

*AXXB*

*CXXD*

*CCDD*

*VVVV*

*VVVV*

for an easy variant of klotski. The letter *V* is reserved for empty slots ("void").

Have fun!