# TypeScript implementation
deno run --allow-net --allow-read --allow-write Main.ts

# C++ implementation
g++ Main.cpp -o results/programcpp
./results/programcpp

# C implementation
gcc Main.c -o results/programc 
./results/programc

# Haskell implementation
ghc -odir results -hidir results Main.hs -o results/programhaskell
./results/programhaskell

# Python implementation
python3 Main.py 

# C# implementation
mcs -out:results/programcsharp Main.cs 
mono results/programcsharp