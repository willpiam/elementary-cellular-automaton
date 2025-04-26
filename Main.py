def rule_to_binary_array(rule_number):
    binary_string = bin(rule_number)[2:].zfill(8)
    return [int(bit) for bit in binary_string]

def calculate_cell(left: int, center: int, right: int, rule: list[int]) -> int:
    # Compute pattern index from left-center-right bits and index rule directly
    pattern = (left << 2) | (center << 1) | right
    return rule[7 - pattern]

def run_cellular_automaton(rule: list[int], generations: int, initial_cells: list[int]) -> list[list[int]]:
    cells = initial_cells.copy()
    ca: list[list[int]] = []

    for _ in range(generations - 1):
        # Add limited padding of 2 zeros to each side
        extended_cells = [0, 0] + cells + [0, 0]
        ca.append(cells)  # Store the current generation

        # Compute next generation using integer neighbors and a list comprehension
        next_generation = [
            calculate_cell(left, center, right, rule)
            for left, center, right in zip(extended_cells, extended_cells[1:], extended_cells[2:])
        ]
        cells = next_generation
    
    ca.append(cells) 
    return ca

def pad_image_data(image_data: list[list[int]], total_width: int) -> list[list[int]]:
    padded_data = []
    for row in image_data:
        padding_length = (total_width - len(row)) // 2
        padding = [0] * padding_length
        padded_row = padding + row + padding
        padded_data.append(padded_row)
    return padded_data

def read_inputs_from_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
        rule_number = int(lines[0].strip())
        initial_conditions = lines[1].strip()
        generations = int(lines[2].strip())
    return rule_number, initial_conditions, generations

def main():
    rule_number, initial_conditions, generations = read_inputs_from_file('input.txt')

    rule_binary = rule_to_binary_array(rule_number)
    cells = [int(bit) for bit in initial_conditions]

    ca = run_cellular_automaton(rule_binary, generations, cells)

    # Determine the total width for the final padding
    final_width = len(initial_conditions) + 2 * generations
    padded_ca = pad_image_data(ca, final_width)

    image_data = f'P1\n{final_width} {generations}\n'
    ca_as_string = '\n'.join(''.join(str(num) for num in row) for row in padded_ca)
    image_data += ca_as_string
    image_data += '\n'
    
    with open(f'results/r{rule_number}_g{generations}_i{initial_conditions}_python.pbm', 'w') as file:
        file.write(image_data)


if __name__ == "__main__":
    main()
