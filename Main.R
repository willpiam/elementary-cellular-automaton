die <- function(msg) {
  cat(msg, "\n", file = stderr())
  quit(save = "no", status = 1)
}

parse_int <- function(text) {
  t <- trimws(text)
  sign <- 1L
  digits <- t
  if (startsWith(t, "-")) {
    sign <- -1L
    digits <- substring(t, 2)
  }
  if (!nzchar(digits) || grepl("[^0-9]", digits)) return(NA_integer_)
  value <- suppressWarnings(as.integer(digits))
  if (is.na(value)) return(NA_integer_)
  if (format(value, scientific = FALSE, trim = TRUE) != sub("^0+", "", digits) &&
      !(value == 0L && grepl("^0+$", digits))) return(NA_integer_)
  as.integer(sign * value)
}

if (!file.exists("input.txt")) die("cannot read input.txt")
lines <- readLines("input.txt", warn = FALSE)
if (length(lines) < 3) die("invalid input")
rule <- parse_int(lines[1])
seed <- sub("\r$", "", lines[2])
generations <- parse_int(lines[3])
if (is.na(rule) || rule < 0L || rule > 255L) die("invalid rule")
if (!nzchar(seed) || grepl("[^01]", seed)) die("invalid initial conditions")
if (is.na(generations) || generations < 1L) die("invalid generations")

n <- nchar(seed)
width <- n + 2L * generations
if (width < n) die("invalid generations")
bits <- integer(8)
for (i in 0:7) bits[i + 1L] <- bitwAnd(bitwShiftR(rule, i), 1L)
even <- bitwAnd(rule, 1L) == 0L
prev <- integer(width)
chars <- strsplit(seed, "", fixed = TRUE)[[1]]
for (i in seq_len(n)) {
  if (chars[i] == "1") prev[generations + i] <- 1L
}

if (!dir.exists("results")) dir.create("results")
path <- sprintf("results/r%d_g%d_i%s_r.pbm", rule, generations, seed)
con <- file(path, open = "wb")
on.exit(close(con), add = TRUE)
cat(sprintf("P1\n%d %d\n", width, generations), file = con, sep = "")

for (gen in 0:(generations - 1L)) {
  cat(paste0(prev, collapse = ""), "\n", file = con, sep = "")
  if (gen + 1L == generations) break
  nxt <- integer(width)
  if (even) {
    start_j <- generations - gen
    end_j <- generations + n + gen + 1L
  } else {
    start_j <- 1L
    end_j <- width
  }
  idx <- start_j:end_j
  left <- integer(length(idx))
  right <- integer(length(idx))
  inside_l <- idx > 1L
  inside_r <- idx < width
  left[inside_l] <- prev[idx[inside_l] - 1L]
  right[inside_r] <- prev[idx[inside_r] + 1L]
  nxt[idx] <- bits[left * 4L + prev[idx] * 2L + right + 1L]
  prev <- nxt
}
