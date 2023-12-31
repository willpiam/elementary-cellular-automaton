def rule_to_binary_array(rule_number):
    binary_string = bin(rule_number)[2:].zfill(8)
    return [int(bit) for bit in binary_string]

def calculate_cell(p_state, rule):
    rule_map = {
        "111": rule[0],
        "110": rule[1],
        "101": rule[2],
        "100": rule[3],
        "011": rule[4],
        "010": rule[5],
        "001": rule[6],
        "000": rule[7]
    }
    return rule_map[p_state]

def run_cellular_automaton(rule_number, generations, initial_conditions):
    cells = [int(bit) for bit in initial_conditions]

    rule_binary = rule_to_binary_array(rule_number)

    # Calculate image width: initial conditions length + 2 cells for each generation
    image_width = len(cells) + 2 * generations
    image_data = f'P1\n{image_width} {generations}\n'

    import time
    start_time = time.perf_counter()

    for i in range(generations):
        # Calculate padding to center the cells
        padding_length = (image_width - len(cells)) // 2
        padding = [0] * padding_length
        extended_cells = padding + cells + padding

        image_data += ''.join([str(cell) if cell else '0' for cell in extended_cells]) + '\n'

        next_generation = []
        for j in range(1, len(extended_cells) - 1):
            left_neighbor = extended_cells[j - 1]
            current_cell = extended_cells[j]
            right_neighbor = extended_cells[j + 1]
            neighborhood = f'{left_neighbor}{current_cell}{right_neighbor}'
            next_generation.append(calculate_cell(neighborhood, rule_binary))
        cells = next_generation

    end_time = time.perf_counter()
    print(f'Took {end_time - start_time:.2f}ms to generate {generations} generations of rule {rule_number}')

    with open(f'results/r{rule_number}_g{generations}_i{initial_conditions}_python.pbm', 'w') as file:
        file.write(image_data)

def read_inputs_from_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
        rule_number = int(lines[0].strip())
        initial_conditions = lines[1].strip()
        generations = int(lines[2].strip())
    return rule_number, initial_conditions, generations

# Main function to run the program
def main():
    rule_number, initial_conditions, generations = read_inputs_from_file('input.txt')
    print(f'Rule Number: {rule_number}')
    print(f'Initial Conditions: {initial_conditions}')
    print(f'Generations: {generations}')

    run_cellular_automaton(rule_number, generations, initial_conditions)

if __name__ == "__main__":
    main()
