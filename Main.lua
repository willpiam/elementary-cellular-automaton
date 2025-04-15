-- Function to convert rule number to binary array
local function ruleToBinaryArray(ruleNumber)
    local ruleBinary = {}
    for i = 0, 7 do
        ruleBinary[i + 1] = (ruleNumber >> i) & 1
    end
    return ruleBinary
end

-- Function to calculate next cell state using bit operations
local function calculateCell(left, center, right, rule)
    local index = (left << 2) | (center << 1) | right
    return rule[index + 1]
end

-- Function to run the cellular automaton
local function runCellularAutomaton(rule, generations, initialCells)
    local initialLength = #initialCells
    local imageWidth = initialLength + 2 * generations
    local ca = {}
    
    -- Pre-allocate arrays
    for i = 1, generations do
        ca[i] = {}
        for j = 1, imageWidth do
            ca[i][j] = 0
        end
    end
    
    -- Initialize first generation
    local paddingLength = (imageWidth - initialLength) / 2
    for i = 1, initialLength do
        ca[1][paddingLength + i] = tonumber(initialCells:sub(i, i))
    end
    
    -- Calculate subsequent generations
    for i = 2, generations do
        local prevRow = ca[i-1]
        local currRow = ca[i]
        
        -- Calculate next generation
        for j = 2, imageWidth - 1 do
            currRow[j] = calculateCell(prevRow[j-1], prevRow[j], prevRow[j+1], rule)
        end
    end
    
    return ca
end

-- Function to read inputs from file
local function readInputsFromFile(filePath)
    local file = io.open(filePath, "r")
    if not file then
        error("Error opening input file!")
    end

    local ruleNumber = tonumber(file:read("*line"))
    local initialConditions = file:read("*line")
    local generations = tonumber(file:read("*line"))
    file:close()

    return ruleNumber, initialConditions, generations
end

-- Main function
local function main()
    local ruleNumber, initialConditions, generations = readInputsFromFile("input.txt")
    local rule = ruleToBinaryArray(ruleNumber)
    local ca = runCellularAutomaton(rule, generations, initialConditions)

    -- Create results directory if it doesn't exist
    os.execute("mkdir -p results")

    -- Write to file
    local filename = string.format("results/r%d_g%d_i%s_lua.pbm", 
        ruleNumber, generations, initialConditions)
    local file = io.open(filename, "w")
    if not file then
        error("Error creating output file!")
    end

    local imageWidth = #ca[1]
    file:write(string.format("P1\n%d %d\n", imageWidth, generations))
    
    for i = 1, generations do
        for j = 1, imageWidth do
            file:write(ca[i][j])
        end
        file:write("\n")
    end
    file:close()
end

-- Run the program
main() 