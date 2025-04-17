function rule_to_binary_array(rule_number::Int)::Vector{Int}
    return [((rule_number >> i) & 1) for i in 0:7]
end

function calculate_cell(left::Int, center::Int, right::Int, rule::Vector{Int})::Int
    index = (left << 2) | (center << 1) | right
    return rule[index + 1]
end

function run_cellular_automaton(rule::Vector{Int}, generations::Int, initial_cells::Vector{Int})::Matrix{Int}
    initial_length = length(initial_cells)
    image_width = initial_length + 2 * generations
    automaton_data = zeros(Int, generations, image_width)
    
    # Initialize first generation
    padding_length = (image_width - initial_length) ÷ 2
    automaton_data[1, padding_length+1:padding_length+initial_length] = initial_cells
    
    # Generate subsequent generations
    for i in 2:generations
        for j in 2:image_width-1
            automaton_data[i, j] = calculate_cell(
                automaton_data[i-1, j-1],
                automaton_data[i-1, j],
                automaton_data[i-1, j+1],
                rule
            )
        end
    end
    
    return automaton_data
end

function read_inputs_from_file(file_path::String)::Tuple{Int, String, Int}
    lines = readlines(file_path)
    rule_number = parse(Int, lines[1])
    initial_conditions = lines[2]
    generations = parse(Int, lines[3])
    return (rule_number, initial_conditions, generations)
end

function main()
    # Read input
    rule_number, initial_conditions, generations = read_inputs_from_file("input.txt")
    
    # Convert initial conditions to array of integers
    initial_cells = [parse(Int, c) for c in initial_conditions]
    
    # Run cellular automaton
    rule = rule_to_binary_array(rule_number)
    automaton_data = run_cellular_automaton(rule, generations, initial_cells)
    
    # Create results directory if it doesn't exist
    mkpath("results")
    
    # Write output to file
    output_file = "results/r$(rule_number)_g$(generations)_i$(initial_conditions)_julia.pbm"
    open(output_file, "w") do io
        # Write PBM header
        println(io, "P1")
        println(io, "$(size(automaton_data, 2)) $(size(automaton_data, 1))")
        
        # Write automaton data
        for i in 1:size(automaton_data, 1)
            for j in 1:size(automaton_data, 2)
                print(io, automaton_data[i, j])
            end
            println(io)
        end
    end
end

main() 