import math

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

def run_cellular_automaton(rule: list[int], generations : int, initial_cells : list[int]) -> str :
    cells = initial_cells.copy()

    # Calculate image width: initial conditions length + 2 cells for each generation
    image_width = len(cells) + 2 * generations
    image_data = ''

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
            next_generation.append(calculate_cell(neighborhood, rule))
        print(f'Next Generation: {next_generation}')
        cells = next_generation

    return image_data

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

    rule_binary = rule_to_binary_array(rule_number)

    import time
    start_time = time.perf_counter()

    cells = [int(bit) for bit in initial_conditions]
    ca = run_cellular_automaton(rule_binary, generations, [int(bit) for bit in initial_conditions])
    image_data = f'P1\n{(len(ca) -1)//generations} {generations}\n'
    image_data += ca
    
    with open(f'results/r{rule_number}_g{generations}_i{initial_conditions}_python.pbm', 'w') as file:
        file.write(image_data)
    
    end_time = time.perf_counter()
    print(f'Took {end_time - start_time:.2f}s to generate {generations} generations of rule {rule_number}')

if __name__ == "__main__":
    main()
