function die(msg::String)
    println(stderr, msg)
    exit(1)
end

function parse_int(text::AbstractString)
    t = strip(text)
    sign = 1
    digits = t
    if startswith(t, '-')
        sign = -1
        digits = t[2:end]
    end
    if isempty(digits) || !all(isdigit, digits)
        return nothing
    end
    value = tryparse(Int, digits)
    value === nothing && return nothing
    return sign * value
end

function valid_seed(seed::AbstractString)
    !isempty(seed) && all(c -> c == '0' || c == '1', seed)
end

function main()
    lines = try
        readlines("input.txt")
    catch
        die("cannot read input.txt")
    end
    length(lines) < 3 && die("invalid input")
    rule = parse_int(lines[1])
    (rule === nothing || rule < 0 || rule > 255) && die("invalid rule")
    seed = chomp(lines[2])
    valid_seed(seed) || die("invalid initial conditions")
    generations = parse_int(lines[3])
    (generations === nothing || generations < 1) && die("invalid generations")

    n = length(seed)
    width = n + 2 * generations
    table = UInt8[(rule >> i) & 1 for i in 0:7]
    even = iseven(rule)
    prev = zeros(UInt8, width)
    nextrow = zeros(UInt8, width)
    for (i, ch) in enumerate(seed)
        if ch == '1'
            prev[generations + i] = 0x01
        end
    end

    mkpath("results")
    path = "results/r$(rule)_g$(generations)_i$(seed)_julia.pbm"
    line = Vector{UInt8}(undef, width + 1)
    line[end] = UInt8('\n')
    open(path, "w") do io
        write(io, "P1\n$(width) $(generations)\n")
        for gen in 0:(generations - 1)
            for j in 1:width
                line[j] = prev[j] == 0x00 ? UInt8('0') : UInt8('1')
            end
            write(io, line)
            gen + 1 == generations && break
            fill!(nextrow, 0x00)
            start_j = even ? generations - gen : 1
            end_j = even ? generations + n + gen + 1 : width
            for j in start_j:end_j
                left = j == 1 ? 0x00 : prev[j - 1]
                right = j == width ? 0x00 : prev[j + 1]
                idx = (Int(left) << 2) | (Int(prev[j]) << 1) | Int(right)
                nextrow[j] = table[idx + 1]
            end
            prev, nextrow = nextrow, prev
        end
    end
end

main()
