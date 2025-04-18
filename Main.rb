def rule_to_binary_array(rule_number)
  (0..7).map { |i| (rule_number >> i) & 1 }
end

def calculate_cell(left, center, right, rule)
  index = (left << 2) | (center << 1) | right
  rule[index]
end

def run_cellular_automaton(rule, generations, initial_cells)
  cells = initial_cells.dup
  ca = []

  (generations - 1).times do
    # Add limited padding of 2 zeros to each side
    extended_cells = [0, 0] + cells + [0, 0]
    ca << cells.dup  # Store the current generation

    next_generation = []
    (1...extended_cells.length - 1).each do |j|
      neighborhood = extended_cells[j-1..j+1].join
      next_generation << calculate_cell(
        extended_cells[j-1],
        extended_cells[j],
        extended_cells[j+1],
        rule
      )
    end
    cells = next_generation
  end

  ca << cells
  ca
end

def pad_image_data(image_data, total_width)
  image_data.map do |row|
    padding_length = (total_width - row.length) / 2
    padding = [0] * padding_length
    padding + row + padding
  end
end

def read_inputs_from_file(file_path)
  lines = File.readlines(file_path)
  rule_number = lines[0].strip.to_i
  initial_conditions = lines[1].strip
  generations = lines[2].strip.to_i
  [rule_number, initial_conditions, generations]
end

def main
  rule_number, initial_conditions, generations = read_inputs_from_file('input.txt')

  rule_binary = rule_to_binary_array(rule_number)
  cells = initial_conditions.chars.map(&:to_i)

  ca = run_cellular_automaton(rule_binary, generations, cells)

  # Determine the total width for the final padding
  final_width = initial_conditions.length + 2 * generations
  padded_ca = pad_image_data(ca, final_width)

  # Create results directory if it doesn't exist
  Dir.mkdir('results') unless Dir.exist?('results')

  # Write to file
  File.open("results/r#{rule_number}_g#{generations}_i#{initial_conditions}_ruby.pbm", 'w') do |file|
    file.puts "P1"
    file.puts "#{final_width} #{generations}"
    padded_ca.each do |row|
      file.puts row.join
    end
  end
end

main 