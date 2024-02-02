# elementry cellular automiton

Run TypeScript code:

    deno run --allow-net --allow-read --allow-write Main.ts

Run C++ code:

    g++ Main.cpp -o results/programcpp ; ./results/programcpp

Run C code:

    gcc Main.c -o results/programc ; ./results/programc

Run Haskell code:
    
    ghc -odir results -hidir results Main.hs -o results/programhaskell ; ./results/programhaskell

Run Python code: 

    python3 Main.py 

Run C# code:

    mcs -out:results/programcsharp Main.cs ; mono results/programcsharp

Run Rust code:

    rustc Main.rs -o results/programrust ; ./results/programrust 

Run Scala 3 code:

    scalac -d ./results Main.scala ; scala -cp ./results CellularAutomaton

Run Go code:

    go run Main.go

Run Clojure code:

    clojure Main.clj

Run Java code:

    javac -d results Main.java ; java -cp results Main

Notice:

    You may need to manually create the `results` directory.

Input file format:

    <RULE>
    <INITIAL CONDITIONS>
    <NUMBER OF GENERATIONS>

Input file name:

    input.txt

Output File format:

    r<rule>_g<generations>_i<initial conditions>_<langauge>.pbm

    Example:

    r30_g100_i1000_cpp.pbm

Run All Versions:

    bash runAll.sh

Clear results:

    bash clearResults.sh

Todo: 

- [ ] ensure no memory leaks in C program
- [ ] ensure no memory leaks in C++ program
- [ ] Write a script to run a subset of these programs with various inputs and record their times 

## Testing

**C++** *with Valgrind*
    
    g++ -g Main.cpp -o results/programcpp ; valgrind --leak-check=full  ./results/programcpp

**C** *with Valgrind*

    gcc -g Main.c -o results/programc ; valgrind --leak-check=full ./results/programc

**Haskell** *with profiling on version B*

    ghc -O2 -prof -fprof-auto -rtsopts -odir results -hidir results MainB.hs -o results/programhaskell_B ; ./results/programhaskell_B +RTS -p -poresults/programhaskell_B  -RTS

## Languages To Add

- [ ] Lisp (common lisp)
- [ ] Kotlin
- [ ] Wolfram Language
- [ ] Lean (??)
- [ ] Agda (??)
- [ ] Idris (??)
- [ ] bash

## Compare.py

This program allows you to run multiple versions of the CA program and compare runs. There are three ways to run the program.

With no flags:

    python3 Compare.py

With the average flag:

    python3 Compare.py -avg

Or with the all flag:

    python3 Compare.py --all

When no arguments are supplied the program will run the programs and print how long they each took. It will also save the runs to a file. 

The average flag causes the program to forgo running any of the CA programs and simply display the average execution time of each language. 

The all command causes the program to print all run times found in the file. It will still execute the CAs and those runs will be included in the output. 

## compareResults.sh

This program takes two files and returns true if they hash to the same value. This is useful for comparing the output of two different implementations of the same cellular automaton program.

To run the program:

    bash compareResults.sh <file1> <file2>

The program will either print "same" or "different" to the terminal.

An example of how this program can be chained with Compare.py to rapidly test changes against another implementation:

    python3 Compare.py ; bash compareResults.sh results/r30_g50_i1_c.pbm results/r30_g50_i1_cpp.pbm 

This becomes clunky and annoying when you are testing different inputs (by altering inputs.txt) and need to modify the command each time you change the inputs. It would be better to include this functionality in the Compare.py program.

## Other variations

    It might be fun to write another C++ version with a more functional style

## Tips 

1. Use sha256sum or compareResults.sh to compare two generated images and ensure they are exactly the same

## Improvements 
1. Some versions of the program are doing more computations than necessary as they are computing cells which are unaffected by the initial conditions. The current python implementation does not have the flaw. Because of this all other versions need to be updated to reflect the logic of the python implementation. 

    A checklist of languages to update:
    - [x] Python
    - [x] C++
    - [x] TypeScript
    - [x] C
    - [ ] Haskell
    - [ ] Go
    - [ ] Scala
    - [ ] Rust
    - [ ] Java
    - [ ] Clojure
    - [ ] C#

2. Remove timer from every version of the program and simply rely on the Compare.py program to time the runs. Programs should have no output except for the image file.

    Versions to update:
    - [ ] Python
    - [ ] C++
    - [ ] TypeScript
    - [ ] C
    - [ ] Haskell
    - [ ] Go
    - [ ] Scala
    - [ ] Rust
    - [ ] Java
    - [ ] Clojure
    - [ ] C#