local function die(msg)
    io.stderr:write(msg, "\n")
    os.exit(1)
end

local function parse_int(text)
    local t = text:match("^%s*(.-)%s*$")
    local sign = 1
    local digits = t
    if t:sub(1, 1) == "-" then
        sign = -1
        digits = t:sub(2)
    end
    if digits == "" or not digits:match("^[0-9]+$") then
        return nil
    end
    local value = tonumber(digits)
    if value == nil or value ~= math.floor(value) or value > 2147483647 then
        return nil
    end
    return sign * value
end

local function valid_seed(seed)
    return seed ~= "" and seed:match("^[01]+$") ~= nil
end

local input = io.open("input.txt", "r")
if not input then
    die("cannot read input.txt")
end
local rule_line = input:read("*l")
local seed = input:read("*l")
local gen_line = input:read("*l")
input:close()
if not rule_line or not seed or not gen_line then
    die("invalid input")
end
seed = seed:gsub("\r$", "")

local rule = parse_int(rule_line)
if not rule or rule < 0 or rule > 255 then
    die("invalid rule")
end
if not valid_seed(seed) then
    die("invalid initial conditions")
end
local generations = parse_int(gen_line)
if not generations or generations < 1 then
    die("invalid generations")
end

local n = #seed
local width = n + 2 * generations
local rule_bits = {}
for i = 0, 7 do
    rule_bits[i] = (rule >> i) & 1
end
local even = (rule & 1) == 0
local prev = {}
local next_row = {}
for j = 1, width do
    prev[j] = 0
    next_row[j] = 0
end
for i = 1, n do
    if seed:sub(i, i) == "1" then
        prev[generations + i] = 1
    end
end

os.execute("mkdir -p results")
local path = string.format("results/r%d_g%d_i%s_lua.pbm", rule, generations, seed)
local out = io.open(path, "wb")
if not out then
    die("cannot write output")
end
out:write(string.format("P1\n%d %d\n", width, generations))

local chars = {}
for gen = 0, generations - 1 do
    for j = 1, width do
        chars[j] = prev[j] == 0 and "0" or "1"
    end
    out:write(table.concat(chars), "\n")
    if gen + 1 == generations then
        break
    end
    for j = 1, width do
        next_row[j] = 0
    end
    local start_j, end_j
    if even then
        start_j = generations - gen
        end_j = generations + n + gen + 1
    else
        start_j = 1
        end_j = width
    end
    for j = start_j, end_j do
        local left = j == 1 and 0 or prev[j - 1]
        local right = j == width and 0 or prev[j + 1]
        local idx = (left << 2) | (prev[j] << 1) | right
        next_row[j] = rule_bits[idx]
    end
    prev, next_row = next_row, prev
end
out:close()
