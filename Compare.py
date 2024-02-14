import subprocess
import time
import json
import os
import sys
import matplotlib.pyplot as plt 
from collections import defaultdict
import numpy as np

# Define the sets of commands with labels. Each set contains a label, a compile command, and a run command.
command_sets = [
    # ("C++", "g++ Main.cpp -o results/programcpp", "./results/programcpp"),  
    ("C", "gcc Main.c -o results/programc", "./results/programc"), 
    # ("Go", "", "go run Main.go") , 
    # ("Rust", "rustc Main.rs -o results/programrust", "./results/programrust"),
    # ("Haskell (slow)", "ghc -odir results -hidir results Main.hs -o results/programhaskell", "./results/programhaskell" ),
    # ("Haskell*", "ghc -odir results -hidir results MainB.hs -o results/programhaskell_B", "./results/programhaskell_B"),
    # ("Haskell**", "ghc -odir results -hidir results MainC.hs -o results/programhaskell_C", "./results/programhaskell_C"),
    ("Java", "javac -d results Main.java", "java -cp results Main"),
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
    
    aggregateData = {}
    for label, times in run_times.items():
        aggregateData[label] = (sum(times) / len(times) , len(times))
    
    return aggregateData

def display_average_run_times(averages):
    print("\nAverage Run Times:")
    print(f"{'Language'.ljust(20, ' ')}{'Average Time (s)'.ljust(20, ' ')}{'Number of Runs'}")
    for label, (avg_time, num_runs) in sorted(averages.items(), key=lambda x: x[1]):
        print(f"{label.ljust(20, ' ')}{f'{avg_time:.4f}'.ljust(20, ' ')}{str(num_runs).ljust(20, ' ')}")

def generate_and_save_graph(data):
    
    if not data:
        print("No data available to plot.")
        return

    # Prepare data for plotting
    generations = [run['generations'] for run in data]
    run_times = [run['run_time'] for run in data]

    # Plotting
    plt.figure(figsize=(10, 6))
    plt.scatter(generations, run_times, color='blue', label='Run Time')
    plt.title('Elementry Cellular Automiton: Generations vs Run Time')
    plt.xlabel('Generations')
    plt.ylabel('Run Time (seconds)')
    plt.legend()
    plt.grid(True)

    # Save the plot in the results directory
    plt.savefig(os.path.join("results", "generations_vs_runtime.png"))
    print("Graph has been saved.")

def run_each_command_set(existing_runs):
    # Read inputs from the file
    rule_number, initial_conditions, generations = read_inputs_from_file("input.txt")

    current_runs = []

    # Execute and time each set of commands
    for label, compile_cmd, run_cmd in command_sets:
        print(f"Running {label}...")
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


def generate_and_save_bar_graph(existing_runs):
    # Aggregate runs based on unique configuration
    aggregated_runs = {}
    for run in existing_runs:
        # Create a unique key for each configuration
        config_key = (run['label'], run['rule_number'], run['initial_conditions'], run['generations'])
        if config_key not in aggregated_runs:
            aggregated_runs[config_key] = []
        aggregated_runs[config_key].append(run['run_time'])
    
    # Calculate average run time for each configuration
    config_labels = []
    avg_run_times = []
    for config, times in aggregated_runs.items():
        avg_time = sum(times) / len(times)
        label = f"{config[0]} (Rule {config[1]}, Gen {config[3]})"
        config_labels.append(label)
        avg_run_times.append(avg_time)
    
    # Generate bar graph
    plt.figure(figsize=(12, 8))
    y_pos = np.arange(len(config_labels))
    plt.bar(y_pos, avg_run_times, align='center', alpha=0.7)
    plt.xticks(y_pos, config_labels, rotation='vertical')
    plt.ylabel('Average Run Time (seconds)')
    plt.title('Average Run Times by Configuration')
    plt.tight_layout()  # Adjust layout to not cut off labels

    # Save the plot in the results directory
    plt.savefig(os.path.join("results", "configurations_vs_runtime.png"))
    print("Bar graph has been saved.")


def main():
    # List to store all runs
    existing_runs = read_existing_data(get_results_file_path())

    if '--graph' in sys.argv or '-g' in sys.argv:
        generate_and_save_graph(existing_runs)
        return

    if '--average' in sys.argv or '-avg' in sys.argv:
        aggregateData = calculate_average_run_times(existing_runs)
        display_average_run_times(aggregateData)
        return

    if ('--bar' in sys.argv or '-b' in sys.argv):
        # create a bar graph where the y axis is the run time and the x axis is the run
        # each slot on the x axis is a unique set of (label, rule_number, initial_conditions, generations)
        # the data is aggregated such that each run of the same label, rule_number, initial_conditions, generations is averaged
        # so if there were two runs in C of rule 30, a single active cell as the initial condition, and 100 generations then
        # the two runs would be averaged together and the result would be a single bar on the x axis 
        generate_and_save_bar_graph(existing_runs)

        return

    run_each_command_set(existing_runs)
    return


if __name__ == "__main__":
    main()
