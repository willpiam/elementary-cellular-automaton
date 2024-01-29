import subprocess
import time
import json
import os
import sys
from collections import defaultdict

# Define the sets of commands with labels. Each set contains a label, a compile command, and a run command.
command_sets = [
    # ("C++", "g++ Main.cpp -o results/programcpp", "./results/programcpp"),  
    ("C", "gcc Main.c -o results/programc", "./results/programc"), 
    # ("Go", "", "go run Main.go") , 
    # ("Rust", "rustc Main.rs -o results/programrust", "./results/programrust"),
    # ("Haskell (slow)", "ghc -odir results -hidir results Main.hs -o results/programhaskell", "./results/programhaskell" ),
#     ("Haskell*", "ghc -odir results -hidir results MainB.hs -o results/programhaskell_B", "./results/programhaskell_B"),
#     ("Java", "javac -d results Main.java", "java -cp results Main"),
    ("Python", "", "python3 Main.py"),
#     ("TypeScript", "", "deno run --allow-net --allow-read --allow-write Main.ts"),
#     ("Scala", "scalac -d ./results Main.scala", "scala -cp ./results CellularAutomaton")
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

def read_inputs_from_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
        rule_number = int(lines[0].strip())
        initial_conditions = lines[1].strip()
        generations = int(lines[2].strip())
    return rule_number, initial_conditions, generations

# Function to get the path to the results file
def get_results_file_path():
    results_directory = "results"
    return os.path.join(results_directory, "run_data.json")

# Function to read existing data from the results file
def read_existing_data(file_path):
    if os.path.exists(file_path):
        with open(file_path, "r") as file:
            return json.load(file)
    return []

# Function to write data to the results file
def write_data_to_file(file_path, data):
    with open(file_path, "w") as file:
        json.dump(data, file, indent=4)

def calculate_average_run_times(runs):
    run_times = defaultdict(list)
    for run in runs:
        run_times[run['label']].append(run['run_time'])
    
    averages = {}
    for label, times in run_times.items():
        averages[label] = sum(times) / len(times)
    
    return averages

def display_average_run_times(averages):
    print("\nAverage Run Times:")
    print(f"{'Language'.ljust(20, ' ')}Average Time (s)")
    for label, avg_time in sorted(averages.items(), key=lambda x: x[1]):
        print(f"{label.ljust(20, ' ')}{avg_time:.4f}")


def main():
    # List to store all runs
    existing_runs = read_existing_data(get_results_file_path())

    if '--average' in sys.argv or '-avg' in sys.argv:
        averages = calculate_average_run_times(existing_runs)
        display_average_run_times(averages)
        return

    # Read inputs from the file
    rule_number, initial_conditions, generations = read_inputs_from_file("input.txt")

    current_runs = []

    # Execute and time each set of commands
    for label, compile_cmd, run_cmd in command_sets:
        execute_command(compile_cmd)  # Compile without timing
        run_time = time_command(run_cmd)  # Run with timing

        # Create a Run dictionary and add it to the list
        run = {
            "label": label,
            "rule_number": rule_number,
            "initial_conditions": initial_conditions,
            "generations": generations,
            "run_time": run_time
        }
        existing_runs.append(run)
        current_runs.append(run)

    # Write the updated data to the file
    write_data_to_file(get_results_file_path(), existing_runs)

   
    
    # Decide which runs to process based on command line arguments
    def runsToProcess():
        if '-a' in sys.argv or '--all' in sys.argv:
            return existing_runs
        else:
            return current_runs

    runs_to_display = runsToProcess()
    # Print the sorted execution times
    for run in sorted(runs_to_display, key=lambda x: x['run_time']):
        label = "label"
        print(f"{f'{run[label]} '.ljust(20, '.')} {run['run_time']:.4f} seconds")

if __name__ == "__main__":
    main()
