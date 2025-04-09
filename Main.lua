-- Function to convert rule number to binary array
local function ruleToBinaryArray(ruleNumber)
    local ruleBinary = {}
    for i = 0, 7 do
        ruleBinary[i + 1] = (ruleNumber >> i) & 1
    end
    return ruleBinary
end

-- Function to calculate next cell state
local function calculateCell(neighborhood, rule)
    local index = tonumber(neighborhood, 2)
    return rule[index + 1]
end

-- Function to run the cellular automaton
local function runCellularAutomaton(rule, generations, initialCells)
    local cells = {}
    for i = 1, #initialCells do
        cells[i] = tonumber(initialCells:sub(i, i))
    end

    local ca = {}
    local imageWidth = #cells + 2 * generations

    for i = 1, generations do
        local paddingLength = (imageWidth - #cells) / 2
        local extendedCells = {}
        
        -- Add padding
        for j = 1, paddingLength do
            extendedCells[j] = 0
        end
        
        -- Add cells
        for j = 1, #cells do
            extendedCells[paddingLength + j] = cells[j]
        end
        
        -- Add padding
        for j = 1, paddingLength do
            extendedCells[paddingLength + #cells + j] = 0
        end

        ca[i] = extendedCells

        -- Calculate next generation
        local nextGeneration = {}
        for j = 2, #extendedCells - 1 do
            local neighborhood = string.format("%d%d%d", 
                extendedCells[j - 1], 
                extendedCells[j], 
                extendedCells[j + 1])
            nextGeneration[j - 1] = calculateCell(neighborhood, rule)
        end
        cells = nextGeneration
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