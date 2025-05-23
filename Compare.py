from builtins import print
import subprocess
import time
import json
import os
import hashlib
import matplotlib
import matplotlib.pyplot as plt 
from collections import defaultdict
import numpy as np
import argparse
import os
import shutil
import platform
import psutil

def load_command_sets():
    try:
        with open('implementations.json', 'r') as f:
            return json.load(f)
    except FileNotFoundError:
        print("Error: implementations.json not found")
        return []
    except json.JSONDecodeError:
        print("Error: implementations.json is not valid JSON")
        return []

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

def generate_and_save_graph(data ):
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

def run_each_command_set(existing_runs, only_top=False, only_version=None):
    rule_number, initial_conditions, generations = read_inputs_from_file("input.txt")
    hashes = defaultdict(list)
    command_sets = load_command_sets()

    for implementation in command_sets:
        if not implementation['enabled']:
            continue
        if only_top and not implementation['is_top']:
            continue
        if only_version and implementation['version_indicator'] != only_version:
            continue

        label = implementation['label']
        version_indicator = implementation['version_indicator']
        compile_cmd = implementation['commands']['build']
        run_cmd = implementation['commands']['run']

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
            file_hash = None
            continue

        run = {
            "label": label,
            "rule_number": rule_number,
            "initial_conditions": initial_conditions,
            "generations": generations,
            "run_time": run_time,
            "image_hash": file_hash
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

def generate_and_save_bar_graph(existing_runs, sort):
    # Check if all runs have the same rule, generation count, and initial conditions
    unique_configs = set((run['rule_number'], run['initial_conditions'], run['generations']) for run in existing_runs)
    uniform_config = len(unique_configs) == 1

    # Aggregate runs based on unique configuration
    aggregated_runs = {}
    label_colors = {}  # Store colors for each label
    colormap = matplotlib.colormaps['hsv']

    for run in existing_runs:
        label = run['label']
        config_key = (label, run['rule_number'], run['initial_conditions'], run['generations'])
        if config_key not in aggregated_runs:
            aggregated_runs[config_key] = []

        if label not in label_colors:
            label_hash = int(hashlib.sha256(label.encode('utf-8')).hexdigest(), 16) % (10**8)
            label_colors[label] = colormap(label_hash / (10**8))

        aggregated_runs[config_key].append(run['run_time'])

    # Calculate average run time for each configuration
    config_avg_times = []
    for config, times in aggregated_runs.items():
        avg_time = sum(times) / len(times)
        if uniform_config:
            label = config[0]  # Use only label if configurations are uniform
        else:
            label = f"{config[0]} (Rule {config[1]}, Gen {config[3]}, IC {config[2]})"
        config_avg_times.append((label, avg_time, label_colors[config[0]]))

    if sort:
        config_avg_times.sort(key=lambda x: x[1])

    config_labels, avg_run_times, colors = zip(*config_avg_times)

    plt.figure(figsize=(12, 8))
    y_pos = np.arange(len(config_labels))
    plt.bar(y_pos, avg_run_times, align='center', alpha=0.7, color=colors)
    plt.xticks(y_pos, config_labels, rotation='vertical')
    plt.ylabel('Average Run Time (seconds)')

    if uniform_config:
        rule, ic, generations = unique_configs.pop()
        plt.title(f'Average Run Times (Rule {rule}, {generations} generations, Initial Conditions="{ic}")')
    else:
        plt.title('Average Run Times by Configuration')

    plt.tight_layout()

    if not os.path.exists("results"):
        os.makedirs("results")

    plt.savefig(os.path.join("results", "configurations_vs_runtime.png"))
    print("Bar graph has been saved.")

def display_system_info():
    print("\nSystem Information:")
    print(f"Operating System: {platform.system()} {platform.release()}")
    print(f"Machine: {platform.machine()}")
    
    # Enhanced CPU Information
    print("\nCPU Information:")
    with open('/proc/cpuinfo', 'r') as f:
        cpu_info = f.read()
        model_name = next((line.split(':')[1].strip() for line in cpu_info.split('\n') if 'model name' in line), 'Unknown')
        print(f"Model: {model_name}")
    
    print(f"CPU Cores: {psutil.cpu_count(logical=False)} physical, {psutil.cpu_count(logical=True)} logical")
    print(f"CPU Frequency: {psutil.cpu_freq().current:.2f} MHz (Min: {psutil.cpu_freq().min:.2f} MHz, Max: {psutil.cpu_freq().max:.2f} MHz)")
    print(f"CPU Usage: {psutil.cpu_percent()}%")
    
    # Memory Information
    print("\nMemory Information:")
    memory = psutil.virtual_memory()
    print(f"Total RAM: {memory.total / (1024**3):.2f} GB")
    print(f"Available RAM: {memory.available / (1024**3):.2f} GB")
    print(f"Used RAM: {memory.used / (1024**3):.2f} GB ({memory.percent}%)")
    print("")
    

def main():
    display_system_info()
    parser = argparse.ArgumentParser(description='Your program description.')

    parser.add_argument('--graph', '-g', action='store_true', help='Generate and save a graph')
    parser.add_argument('--average', '-avg', action='store_true', help='Calculate and display average run times')
    parser.add_argument('--bar', '-b', action='store_true', help='Generate and save a bar graph')
    parser.add_argument('--sort', action='store_true', help='Sort the bar graph')
    parser.add_argument('--runs', type=int, help='Specify the number of runs')
    parser.add_argument('--clear', action='store_true', help='Clear the results directory')
    parser.add_argument('--only-top', action='store_true', help='Only run implementations marked as top performers')
    parser.add_argument('--only', type=str, help='Only run the implementation with the specified version indicator')

    args = parser.parse_args()
    # List to store all runs
    existing_runs = read_existing_data(get_results_file_path())

    if args.graph:
        generate_and_save_graph(existing_runs)
        return

    if args.average:
        aggregateData = calculate_average_run_times(existing_runs)
        display_average_run_times(aggregateData)
        return

    if args.bar:
        generate_and_save_bar_graph(existing_runs, args.sort)
        return

    if args.clear:
        if os.path.exists("results"):
            shutil.rmtree("results")
            os.makedirs("results")
        return

    for i in range(0, args.runs if args.runs is not None else 1):
        run_each_command_set(existing_runs, args.only_top, args.only)

    return


if __name__ == "__main__":
    main()
