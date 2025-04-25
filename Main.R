############################################
# Elementary Cellular Automaton in R       #
#                                          #
# Reads:  input.txt                        #
#   line 1: rule number (0-255)            #
#   line 2: initial conditions as 0/1 string#
#   line 3: number of generations          #
#                                          #
# Writes: results/r{rule}_g{gen}_i{init}_r.pbm
############################################

# Convert rule number to binary vector (length 8, LSB first)
rule_to_binary <- function(rule_number) {
  bits <- integer(8)
  for (i in 0:7) {
    bits[i + 1] <- bitwAnd(bitwShiftR(rule_number, i), 1)
  }
  bits
}

# Calculate next cell state from neighborhood
calculate_cell <- function(left, center, right, rule_bits) {
  idx <- left * 4 + center * 2 + right          # pattern encoded as integer 0-7
  rule_bits[idx + 1]                            # R is 1-based
}

# Run the cellular automaton, returning a matrix (generations x width)
run_cellular_automaton <- function(rule_number, generations, initial_conditions) {
  initial_cells <- as.integer(strsplit(initial_conditions, "")[[1]])
  rule_bits <- rule_to_binary(rule_number)

  image_width <- length(initial_cells) + 2 * generations
  automaton <- matrix(0L, nrow = generations, ncol = image_width)

  # Initialise first generation centred in the row
  offset <- (image_width - length(initial_cells)) / 2
  automaton[1, (offset + 1):(offset + length(initial_cells))] <- initial_cells

  # Generate successive generations
  for (gen in 2:generations) {
    prev <- automaton[gen - 1, ]
    curr <- integer(image_width)
    for (j in seq_len(image_width)) {
      left   <- if (j == 1) 0 else prev[j - 1]
      center <- prev[j]
      right  <- if (j == image_width) 0 else prev[j + 1]
      curr[j] <- calculate_cell(left, center, right, rule_bits)
    }
    automaton[gen, ] <- curr
  }

  automaton
}

# Write PBM file with results
output_to_file <- function(automaton, rule_number, generations, initial_conditions) {
  if (!dir.exists("results")) dir.create("results")
  file_name <- sprintf("results/r%d_g%d_i%s_r.pbm", rule_number, generations, initial_conditions)
  con <- file(file_name, open = "w")
  on.exit(close(con))

  width <- ncol(automaton)
  height <- nrow(automaton)
  cat(sprintf("P1\n%d %d\n", width, height), file = con)
  apply(automaton, 1, function(row) {
    cat(paste(row, collapse = ""), "\n", file = con, sep = "")
  })
}

# ------------------- main ------------------

main <- function() {
  input <- readLines("input.txt")
  if (length(input) < 3) {
    stop("input.txt must contain at least 3 lines (rule, initial conditions, generations)")
  }
  rule_number <- as.integer(input[1])
  initial_conditions <- gsub("\\s", "", input[2])  # remove whitespace
  generations <- as.integer(input[3])

  automaton <- run_cellular_automaton(rule_number, generations, initial_conditions)
  output_to_file(automaton, rule_number, generations, initial_conditions)
}

# Execute when the script is run directly
if (sys.nframe() == 0) {
  main()
} 