def die(msg)
  warn msg
  exit 1
end

def parse_int(text)
  t = text.strip
  sign = 1
  digits = t
  if t.start_with?('-')
    sign = -1
    digits = t[1..]
  end
  return nil if digits.nil? || digits.empty? || digits !~ /\A\d+\z/
  sign * digits.to_i
end

def read_inputs(path)
  lines = File.readlines(path, chomp: true)
  die('invalid input') if lines.length < 3
  rule = parse_int(lines[0])
  seed = lines[1]
  generations = parse_int(lines[2])
  die('invalid rule') if rule.nil? || rule < 0 || rule > 255
  die('invalid initial conditions') if seed.empty? || seed !~ /\A[01]+\z/
  die('invalid generations') if generations.nil? || generations < 1
  [rule, seed, generations]
rescue Errno::ENOENT
  die('cannot read input.txt')
end

def run(rule, generations, seed, file)
  n = seed.length
  width = n + 2 * generations
  table = (0..7).map { |i| (rule >> i) & 1 }
  even = rule.even?
  prev = Array.new(width, 0)
  nxt = Array.new(width, 0)
  seed.each_char.with_index do |ch, i|
    prev[generations + i] = 1 if ch == '1'
  end
  file.write("P1\n#{width} #{generations}\n")
  generations.times do |gen|
    file.write(prev.map { |cell| cell == 0 ? '0' : '1' }.join)
    file.write("\n")
    break if gen + 1 == generations

    nxt.fill(0)
    lo, hi = if even
               [generations - (gen + 1), generations + n + (gen + 1)]
             else
               [0, width]
             end
    (lo...hi).each do |j|
      left = j > 0 ? prev[j - 1] : 0
      right = j + 1 < width ? prev[j + 1] : 0
      nxt[j] = table[(left << 2) | (prev[j] << 1) | right]
    end
    prev, nxt = nxt, prev
  end
end

def main
  rule, seed, generations = read_inputs('input.txt')
  Dir.mkdir('results') unless Dir.exist?('results')
  path = "results/r#{rule}_g#{generations}_i#{seed}_ruby.pbm"
  File.open(path, 'wb') { |file| run(rule, generations, seed, file) }
end

main
