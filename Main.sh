#!/bin/bash

# Precompute the rule lookup table
declare -a RULE_TABLE

init_rule_table() {
    local rule_number=$1
    for ((i=0; i<8; i++)); do
        RULE_TABLE[i]=$(( (rule_number >> i) & 1 ))
    done
}

# Run the cellular automaton
run_cellular_automaton() {
    local rule_number=$1
    local generations=$2
    local initial_state=$3
    local width=${#initial_state}
    local final_width=$((width + 2*generations))
    
    # Initialize rule table
    init_rule_table "$rule_number"
    
    # Create output directory
    mkdir -p results
    
    # Open output file for writing
    local output_file="results/r${rule_number}_g${generations}_i${initial_state}_bash.pbm"
    exec 3> "$output_file"
    
    # Write PBM header
    echo "P1" >&3
    echo "$final_width $generations" >&3
    
    # Convert initial state to array for faster access
    declare -a current_gen
    for ((i=0; i<width; i++)); do
        current_gen[i]=${initial_state:i:1}
    done
    
    # Output first generation (padded)
    local half_padding=$(( (final_width - width) / 2 ))
    local row=""
    for ((i=0; i<half_padding; i++)); do
        row+="0"
    done
    for ((i=0; i<width; i++)); do
        row+="${current_gen[i]}"
    done
    for ((i=0; i<half_padding; i++)); do
        row+="0"
    done
    echo "$row" >&3
    
    # Process each subsequent generation
    for ((gen=1; gen<generations; gen++)); do
        # Expand current generation array with padding
        declare -a padded_gen
        padded_gen[0]=0
        padded_gen[1]=0
        for ((i=0; i<width; i++)); do
            padded_gen[i+2]=${current_gen[i]}
        done
        padded_gen[width+2]=0
        padded_gen[width+3]=0
        
        # Calculate next generation
        declare -a next_gen
        for ((i=0; i<width+2; i++)); do
            # Calculate neighborhood index directly
            local idx=$(( padded_gen[i]*4 + padded_gen[i+1]*2 + padded_gen[i+2] ))
            next_gen[i]=${RULE_TABLE[idx]}
        done
        
        # Update width and current generation
        width=$((width+2))
        unset current_gen
        declare -a current_gen=("${next_gen[@]}")
        
        # Output this generation (padded)
        local remaining_padding=$(( (final_width - width) / 2 ))
        row=""
        for ((i=0; i<remaining_padding; i++)); do
            row+="0"
        done
        for ((i=0; i<width; i++)); do
            row+="${current_gen[i]}"
        done
        for ((i=0; i<remaining_padding; i++)); do
            row+="0"
        done
        echo "$row" >&3
    done
    
    # Close output file
    exec 3>&-
}

# Main function
main() {
    # Read input from file
    local input_file="input.txt"
    if [ ! -f "$input_file" ]; then
        echo "Error: input.txt not found"
        exit 1
    fi
    
    # Read the first three lines
    mapfile -t input_lines < "$input_file"
    local rule_number=${input_lines[0]}
    local initial_conditions=${input_lines[1]}
    local generations=${input_lines[2]}
    
    # Run the cellular automaton
    run_cellular_automaton "$rule_number" "$generations" "$initial_conditions"
}

# Run the main function
main