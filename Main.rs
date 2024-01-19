use std::fs::File;
use std::io::{self, Write, BufRead};
use std::path::Path;
use std::time::{Instant};

fn rule_to_binary_array(rule_number: u8) -> Vec<u8> {
    (0..8).map(|i| (rule_number >> i) & 1).collect()
}

fn calculate_cell(neighborhood: &str, rule_binary: &[u8]) -> u8 {
    let index = u8::from_str_radix(neighborhood, 2).unwrap();
    rule_binary[index as usize]
}

fn run_cellular_automaton(rule_number: u8, generations: usize, initial_conditions: &str) -> Vec<Vec<u8>> {
    let mut cells: Vec<u8> = initial_conditions.chars().map(|c| if c == '1' { 1 } else { 0 }).collect();
    let rule_binary = rule_to_binary_array(rule_number);
    let image_width = cells.len() + 2 * generations;
    let mut automaton_data = Vec::new();

    for _ in 0..generations {
        let padding_length = (image_width - cells.len()) / 2;
        let padding = vec![0; padding_length];
        let mut extended_cells = padding.clone();
        extended_cells.extend(&cells);
        extended_cells.extend(padding);

        automaton_data.push(extended_cells.clone());

        let mut next_generation = vec![0; extended_cells.len()];
        for j in 0..extended_cells.len() {
            let left_neighbor = if j > 0 { extended_cells[j - 1] } else { 0 };
            let current_cell = extended_cells[j];
            let right_neighbor = if j < extended_cells.len() - 1 { extended_cells[j + 1] } else { 0 };
            let neighborhood = format!("{}{}{}", left_neighbor, current_cell, right_neighbor);
            next_generation[j] = calculate_cell(&neighborhood, &rule_binary);
        }
        cells = next_generation;
    }

    automaton_data
}

fn output_to_file(automaton_data: &[Vec<u8>], rule_number: u8, generations: usize, initial_conditions: &str) -> io::Result<()> {
    let image_width = automaton_data.first().map_or(0, Vec::len);

    let mut file = File::create(format!("results/r{}_g{}_i{}_rust.pbm", rule_number, generations, initial_conditions))?;
    writeln!(file, "P1\n{} {}", image_width, generations)?;

    for row in automaton_data {
        for &cell in row {
            write!(file, "{}", if cell == 1 { '1' } else { '0' })?;
        }
        writeln!(file)?;
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let input_path = Path::new("input.txt");
    let input_file = File::open(&input_path)?;
    let mut lines = io::BufReader::new(input_file).lines();

    let rule_number = lines.next().unwrap()?.parse().unwrap();
    let initial_conditions = lines.next().unwrap()?;
    let generations = lines.next().unwrap()?.parse().unwrap();

    let start = Instant::now();

    let automaton_data = run_cellular_automaton(rule_number, generations, &initial_conditions);
    output_to_file(&automaton_data, rule_number, generations, &initial_conditions)?;

    let duration = start.elapsed();
    println!("Took {:?} to generate {} generations of rule {}", duration, generations, rule_number);

    Ok(())
}
