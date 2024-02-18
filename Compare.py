from builtins import print
import subprocess
import time
import json
import os
import sys
import hashlib
import matplotlib.pyplot as plt 
from collections import defaultdict
import numpy as np

command_sets = [
    ("C", "c", "gcc Main.c -o results/programc", "./results/programc"),
    ("C++", "cpp", "g++ Main.cpp -o results/programcpp", "./results/programcpp"),
    ("Java", "java", "javac -d results Main.java", "java -cp results Main"),
    ("Python", "python", "", "python3 Main.py"),
    ("TypeScript", "typescript", "", "deno run --allow-net --allow-read --allow-write Main.ts"),
    ("C#", "csharp", "mcs -out:results/programcsharp Main.cs", "mono results/programcsharp"),

    ("Rust", "rust", "rustc Main.rs -o results/programrust", "./results/programrust"),
    ("Go", "go", "", "go run Main.go"),
    ("Haskell", "haskell", "ghc -odir results -hidir results Main.hs -o results/programhaskell", "./results/programhaskell"),
    # ("Haskell**", "ghc -odir results -hidir results MainC.hs -o results/programhaskell_C", "./results/programhaskell_C"),
    ("Scala", "scala", "scalac -d ./results Main.scala", "scala -cp ./results CellularAutomaton"),
    ("Clojure", "clojure", "", "clojure Main.clj"),
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
    rule_number, initial_conditions, generations = read_inputs_from_file("input.txt")
    hashes = defaultdict(list)

    for label, version_indicator, compile_cmd, run_cmd in command_sets:
        print(f"Running {label}...")
        execute_command(compile_cmd)
        run_time = time_command(run_cmd)

        filename = f"results/r{rule_number}_g{generations}_i{initial_conditions}_{version_indicator}.pbm"
        if os.path.exists(filename):
            with open(filename, 'rb') as file:
                file_contents = file.read()
                file_hash = hashlib.sha256(file_contents).hexdigest()
            hashes[file_hash].append(label)
        else:
            print(f"File {filename} not found for {label}")
        
        run = {
            "label": label,
            "rule_number": rule_number,
            "initial_conditions": initial_conditions,
            "generations": generations,
            "run_time": run_time
        }
        existing_runs.append(run)

    write_data_to_file(get_results_file_path(), existing_runs)

    if len(hashes) == 1:
        print(f"\nAll generated images hash to {next(iter(hashes.keys()))}")
    else:
        print(f"\nWarning! The generated images hash to the following {len(hashes)} unique values:\n")
        for file_hash, labels in hashes.items():
            print(f"{file_hash}")
            for label in labels:
                print(f"\t{label}")


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
        label = f"{config[0]} (Rule {config[1]}, Gen {config[3]}, IC {config[2]})"
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
        generate_and_save_bar_graph(existing_runs)
        return

    run_each_command_set(existing_runs)
    return


if __name__ == "__main__":
    main()
