import subprocess
import time

# Define the sets of commands with labels. Each set contains a label, a compile command, and a run command.
command_sets = [
    # ("C++", "g++ Main.cpp -o results/programcpp", "./results/programcpp"),  
    ("C", "gcc Main.c -o results/programc", "./results/programc"), 
    # ("Go", "", "go run Main.go") , 
    # ("Rust", "rustc Main.rs -o results/programrust", "./results/programrust"),
    # ("Haskell (slow)", "ghc -odir results -hidir results Main.hs -o results/programhaskell", "./results/programhaskell" ),
    ("Haskell*", "ghc -odir results -hidir results MainB.hs -o results/programhaskell_B", "./results/programhaskell_B"),
    # ("Java", "javac -d results Main.java", "java -cp results Main"),
    ("Python", "", "python3 Main.py"),
    # ("TypeScript", "", "deno run --allow-net --allow-read --allow-write Main.ts"),
    # ("Scala", "scalac -d ./results Main.scala", "scala -cp ./results CellularAutomaton")
]

# Function to execute a command (no timing)
def execute_command(command):
    if command:  # Only run if the command is not an empty string
        subprocess.run(command, shell=True)  # Using shell=True to interpret the command as a shell command

# Function to execute a command and measure its execution time
def time_command(command):
    start_time = time.time()
    subprocess.run(command, shell=True)
    end_time = time.time()
    return end_time - start_time

# Execute and time each set of commands
execution_times = {}
for label, compile_cmd, run_cmd in command_sets:
    execute_command(compile_cmd)  # Compile without timing
    execution_times[label] = time_command(run_cmd)  # Run with timing

# Sort the execution times by their duration
sorted_execution_times = sorted(execution_times.items(), key=lambda x: x[1])

# Print the sorted execution times
for cmd, duration in sorted_execution_times:
    print(f"{f'{cmd} '.ljust(20, '.')} {duration:.4f} seconds")