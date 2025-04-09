#!/bin/bash

# Function to convert a rule number to its binary representation
rule_to_binary_array() {
    local rule_number=$1
    local binary=""
    for ((i=0; i<8; i++)); do
        binary="$(( (rule_number >> i) & 1 ))$binary"
    done
    echo "$binary"
}

# Function to calculate the next state of a cell based on its neighborhood
calculate_cell() {
    local neighborhood=$1
    local rule=$2
    local index=0
    
    # Convert neighborhood to decimal index
    for ((i=0; i<3; i++)); do
        local bit=${neighborhood:$i:1}
        index=$(( (index << 1) | bit ))
    done
    
    # Get the corresponding bit from the rule
    echo "${rule:$((7-index)):1}"
}

# Function to run the cellular automaton
run_cellular_automaton() {
    local rule=$1
    local generations=$2
    local initial_cells=$3
    
    # Initialize the automaton data
    local cells=$initial_cells
    local automaton_data=()
    automaton_data+=("$cells")
    
    # Run for the specified number of generations
    for ((i=1; i<generations; i++)); do
        # Add padding of 2 zeros to each side
        local padded_cells="00${cells}00"
        local next_generation=""
        
        # Calculate next generation
        for ((j=1; j<${#padded_cells}-1; j++)); do
            local neighborhood=${padded_cells:$((j-1)):3}
            next_generation+=$(calculate_cell "$neighborhood" "$rule")
        done
        
        cells=$next_generation
        automaton_data+=("$cells")
    done
    
    echo "${automaton_data[@]}"
}

# Function to pad the image data
pad_image_data() {
    local image_data=($1)
    local total_width=$2
    local padded_data=()
    
    for row in "${image_data[@]}"; do
        local padding_length=$(( (total_width - ${#row}) / 2 ))
        local padding=$(printf "%0${padding_length}d" 0)
        padded_data+=("${padding}${row}${padding}")
    done
    
    echo "${padded_data[@]}"
}

# Main function
main() {
    # Read input from file
    local input_file="input.txt"
    if [ ! -f "$input_file" ]; then
        echo "Error: input.txt not found"
        exit 1
    fi
    
    local rule_number
    local initial_conditions
    local generations
    
    read -r rule_number < "$input_file"
    read -r initial_conditions < <(tail -n +2 "$input_file" | head -n 1)
    read -r generations < <(tail -n +3 "$input_file" | head -n 1)
    
    # Convert rule number to binary
    local rule_binary=$(rule_to_binary_array "$rule_number")
    
    # Run the cellular automaton
    local automaton_data=$(run_cellular_automaton "$rule_binary" "$generations" "$initial_conditions")
    
    # Calculate final width and pad the data
    local final_width=$(( ${#initial_conditions} + 2 * generations ))
    local padded_data=$(pad_image_data "$automaton_data" "$final_width")
    
    # Create results directory if it doesn't exist
    mkdir -p results
    
    # Write to PBM file
    local output_file="results/r${rule_number}_g${generations}_i${initial_conditions}_bash.pbm"
    {
        echo "P1"
        echo "$final_width $generations"
        for row in $padded_data; do
            echo "$row"
        done
    } > "$output_file"
}

# Run the main function
main 