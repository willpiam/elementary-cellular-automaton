using System;
using System.IO;
using System.Text;

class CellularAutomaton
{
    const int MaxLengthInitialConditions = 1000;

    static bool[] RuleToBinaryArray(int ruleNumber)
    {
        bool[] ruleBinary = new bool[8];
        for (int i = 0; i < 8; i++)
            ruleBinary[i] = (ruleNumber >> i & 1) == 1;
        return ruleBinary;
    }

    static bool CalculateCell(bool left, bool center, bool right, bool[] ruleBinary)
    {
        int index = (left ? 4 : 0) | (center ? 2 : 0) | (right ? 1 : 0);
        return ruleBinary[index];
    }

    static bool[][] RunCellularAutomaton(int rule, int generations, string cells)
    {
        int initialConditionsLength = cells.Length;
        int imageWidth = initialConditionsLength + 2 * generations;
        bool[][] automatonData = new bool[generations][];
        bool[] ruleBinary = RuleToBinaryArray(rule);

        int length = initialConditionsLength;
        int initialOffset = (imageWidth - initialConditionsLength) / 2;

        for (int i = 0; i < generations; i++)
        {
            automatonData[i] = new bool[imageWidth];
            if (i == 0)
            {
                for (int j = 0; j < initialConditionsLength; j++)
                    automatonData[i][j + initialOffset] = cells[j] == '1';
            }
        }

        length += 2;

        for (int i = 1; i < generations; i++)
        {
            int paddingOffset = initialOffset - i;
            for (int j = paddingOffset; j < paddingOffset + length; j++)
            {
                automatonData[i][j] = CalculateCell(
                    automatonData[i - 1][j - 1],
                    automatonData[i - 1][j],
                    automatonData[i - 1][j + 1],
                    ruleBinary
                );
            }
            length += 2;
        }

        return automatonData;
    }

    static void OutputToFile(bool[][] automatonData, int ruleNumber, int generations, string initialConditions)
    {
        string directoryPath = "results";
        Directory.CreateDirectory(directoryPath);
        string filename = $"{directoryPath}/r{ruleNumber}_g{generations}_i{initialConditions}_csharp.pbm";
        
        using (StreamWriter file = new StreamWriter(filename))
        {
            int imageWidth = initialConditions.Length + 2 * generations;
            file.WriteLine($"P1\n{imageWidth} {generations}");
            
            StringBuilder lineBuilder = new StringBuilder(imageWidth);
            foreach (var row in automatonData)
            {
                lineBuilder.Clear();
                foreach (var cell in row)
                    lineBuilder.Append(cell ? '1' : '0');
                file.WriteLine(lineBuilder);
            }
        }
    }

    static void Main()
    {
        try
        {
            string[] input = File.ReadAllLines("input.txt");
            int ruleNumber = int.Parse(input[0]);
            string initialConditions = input[1];
            int generations = int.Parse(input[2]);

            bool[][] automatonData = RunCellularAutomaton(ruleNumber, generations, initialConditions);
            OutputToFile(automatonData, ruleNumber, generations, initialConditions);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"An error occurred: {ex.Message}");
        }
    }
}
