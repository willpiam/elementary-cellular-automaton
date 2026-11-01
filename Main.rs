use std::fs::{self, File};
use std::io::{self, Write};
use std::process;

fn die(msg: &str) -> ! {
    eprintln!("{msg}");
    process::exit(1);
}

fn parse_i32(text: &str) -> Option<i32> {
    let t = text.trim().trim_end_matches('\r');
    let (neg, digits) = if let Some(rest) = t.strip_prefix('-') {
        (true, rest)
    } else {
        (false, t)
    };
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let value: i32 = digits.parse().ok()?;
    Some(if neg { -value } else { value })
}

fn valid_seed(seed: &str) -> bool {
    !seed.is_empty() && seed.bytes().all(|b| b == b'0' || b == b'1')
}

fn main() {
    let text = fs::read_to_string("input.txt").unwrap_or_else(|_| die("cannot read input.txt"));
    let mut lines = text.split('\n');
    let rule_line = lines.next().unwrap_or("");
    let mut seed = lines.next().unwrap_or("").to_string();
    let gen_line = match lines.next() {
        Some(line) => line,
        None => die("invalid input"),
    };
    if seed.ends_with('\r') {
        seed.pop();
    }
    if rule_line.is_empty() && seed.is_empty() && gen_line.is_empty() {
        die("invalid input");
    }

    let rule_number = parse_i32(rule_line).unwrap_or_else(|| die("invalid rule"));
    if !(0..=255).contains(&rule_number) {
        die("invalid rule");
    }
    if !valid_seed(&seed) {
        die("invalid initial conditions");
    }
    let generations = parse_i32(gen_line).unwrap_or_else(|| die("invalid generations"));
    if generations < 1 {
        die("invalid generations");
    }

    let g = generations as usize;
    let n = seed.len();
    let width = g.checked_mul(2).and_then(|v| v.checked_add(n)).unwrap_or_else(|| die("invalid generations"));
    let even = rule_number & 1 == 0;
    let rule: [u8; 8] = std::array::from_fn(|i| ((rule_number >> i) & 1) as u8);

    let mut prev = vec![0u8; width];
    let mut next = vec![0u8; width];
    for (i, byte) in seed.bytes().enumerate() {
        if byte == b'1' {
            prev[g + i] = 1;
        }
    }

    fs::create_dir_all("results").unwrap_or_else(|_| die("cannot create results"));
    let path = format!("results/r{rule_number}_g{generations}_i{seed}_rust.pbm");
    let mut out = io::BufWriter::new(File::create(&path).unwrap_or_else(|_| die("cannot write output")));
    write!(out, "P1\n{width} {generations}\n").unwrap_or_else(|_| die("cannot write output"));

    let mut line = vec![b'0'; width + 1];
    line[width] = b'\n';

    for gen in 0..g {
        for j in 0..width {
            line[j] = if prev[j] == 0 { b'0' } else { b'1' };
        }
        out.write_all(&line).unwrap_or_else(|_| die("cannot write output"));
        if gen + 1 == g {
            break;
        }
        next.fill(0);
        let (lo, hi) = if even {
            (g - (gen + 1), g + n + (gen + 1))
        } else {
            (0, width)
        };
        for j in lo..hi {
            let left = if j > 0 { prev[j - 1] } else { 0 };
            let right = if j + 1 < width { prev[j + 1] } else { 0 };
            let idx = ((left << 2) | (prev[j] << 1) | right) as usize;
            next[j] = rule[idx];
        }
        std::mem::swap(&mut prev, &mut next);
    }
    out.flush().unwrap_or_else(|_| die("cannot write output"));
}
