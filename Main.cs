// using System;
// using System.IO;
// using System.Collections.Generic;
// using System.Diagnostics;
// using System.Text;

// class CellularAutomaton
// {
//     static int[] RuleToBinaryArray(int ruleNumber)
//     {
//         string binaryString = Convert.ToString(ruleNumber, 2).PadLeft(8, '0');
//         int[] binaryArray = new int[8];
//         for (int i = 0; i < 8; i++)
//         {
//             binaryArray[i] = int.Parse(binaryString[i].ToString());
//         }
//         return binaryArray;
//     }

//     static int CalculateCell(string pState, int[] rule)
//     {
//         var ruleMap = new Dictionary<string, int>
//         {
//             { "111", rule[0] },
//             { "110", rule[1] },
//             { "101", rule[2] },
//             { "100", rule[3] },
//             { "011", rule[4] },
//             { "010", rule[5] },
//             { "001", rule[6] },
//             { "000", rule[7] }
//         };
//         return ruleMap[pState];
//     }

//     static void RunCellularAutomaton(int ruleNumber, int generations, string initialConditions)
//     {
//         List<int> cells = new List<int>();
//         foreach (char c in initialConditions)
//         {
//             cells.Add(int.Parse(c.ToString()));
//         }

//         int[] ruleBinary = RuleToBinaryArray(ruleNumber);
//         int imageWidth = cells.Count + 2 * generations;
//         StringBuilder imageData = new StringBuilder();
//         imageData.AppendLine($"P1\n{imageWidth} {generations}");

//         Stopwatch stopwatch = Stopwatch.StartNew();

//         for (int i = 0; i < generations; i++)
//         {
//             int paddingLength = (imageWidth - cells.Count) / 2;
//             List<int> padding = new List<int>(new int[paddingLength]);
//             List<int> extendedCells = new List<int>();
//             extendedCells.AddRange(padding);
//             extendedCells.AddRange(cells);
//             extendedCells.AddRange(padding);

//             foreach (var cell in extendedCells)
//             {
//                 imageData.Append(cell > 0 ? "1" : "0");
//             }
//             imageData.AppendLine();

//             List<int> nextGeneration = new List<int>();
//             for (int j = 1; j < extendedCells.Count - 1; j++)
//             {
//                 string neighborhood = $"{extendedCells[j - 1]}{extendedCells[j]}{extendedCells[j + 1]}";
//                 nextGeneration.Add(CalculateCell(neighborhood, ruleBinary));
//             }
//             cells = nextGeneration;
//         }

//         stopwatch.Stop();
//         Console.WriteLine($"Took {stopwatch.ElapsedMilliseconds}ms to generate {generations} generations of rule {ruleNumber}");

//         File.WriteAllText($"results/r{ruleNumber}_g{generations}_i{initialConditions}_csharp.pbm", imageData.ToString());
//     }

//     static void Main(string[] args)
//     {
//         string[] lines = File.ReadAllLines("input.txt");
//         int ruleNumber = int.Parse(lines[0].Trim());
//         string initialConditions = lines[1].Trim();
//         int generations = int.Parse(lines[2].Trim());

//         Console.WriteLine($"Rule Number: {ruleNumber}");
//         Console.WriteLine($"Initial Conditions: {initialConditions}");
//         Console.WriteLine($"Generations: {generations}");

//         RunCellularAutomaton(ruleNumber, generations, initialConditions);
//     }
// }

using System;
using System.Collections.Generic;
using System.IO;

class CellularAutomaton
{
    static List<int> RuleToBinaryArray(int ruleNumber)
    {
        string binaryString = Convert.ToString(ruleNumber, 2).PadLeft(8, '0');
        List<int> binaryArray = new List<int>();
        foreach (char bit in binaryString)
        {
            binaryArray.Add(int.Parse(bit.ToString()));
        }
        return binaryArray;
    }

    static int CalculateCell(string pState, List<int> rule)
    {
        Dictionary<string, int> ruleMap = new Dictionary<string, int>
        {
            {"111", rule[0]},
            {"110", rule[1]},
            {"101", rule[2]},
            {"100", rule[3]},
            {"011", rule[4]},
            {"010", rule[5]},
            {"001", rule[6]},
            {"000", rule[7]}
        };
        return ruleMap[pState];
    }

    static List<List<int>> RunCellularAutomaton(List<int> rule, int generations, List<int> initialCells)
    {
        List<int> cells = new List<int>(initialCells);
        List<List<int>> ca = new List<List<int>>();

        for (int i = 0; i < generations - 1; i++)
        {
            List<int> extendedCells = new List<int> { 0, 0 };
            extendedCells.AddRange(cells);
            extendedCells.Add(0);
            extendedCells.Add(0);
            ca.Add(new List<int>(cells));

            List<int> nextGeneration = new List<int>();
            for (int j = 1; j < extendedCells.Count - 1; j++)
            {
                string neighborhood = string.Join("", extendedCells.GetRange(j - 1, 3).ConvertAll(k => k.ToString()));
                nextGeneration.Add(CalculateCell(neighborhood, rule));
            }
            cells = nextGeneration;
        }
        ca.Add(cells);
        return ca;
    }

    static List<List<int>> PadImageData(List<List<int>> imageData, int totalWidth)
    {
        List<List<int>> paddedData = new List<List<int>>();
        foreach (var row in imageData)
        {
            int paddingLength = (totalWidth - row.Count) / 2;
            List<int> paddedRow = new List<int>(new int[paddingLength]);
            paddedRow.AddRange(row);
            paddedRow.AddRange(new int[paddingLength]);
            paddedData.Add(paddedRow);
        }
        return paddedData;
    }

    static (int, string, int) ReadInputsFromFile(string filePath)
    {
        string[] lines = File.ReadAllLines(filePath);
        int ruleNumber = int.Parse(lines[0].Trim());
        string initialConditions = lines[1].Trim();
        int generations = int.Parse(lines[2].Trim());
        return (ruleNumber, initialConditions, generations);
    }

    static void Main(string[] args)
    {
        (int ruleNumber, string initialConditions, int generations) = ReadInputsFromFile("input.txt");
        
        List<int> ruleBinary = RuleToBinaryArray(ruleNumber);
        List<int> cells = new List<int>();
        foreach (char bit in initialConditions)
        {
            cells.Add(int.Parse(bit.ToString()));
        }

        List<List<int>> ca = RunCellularAutomaton(ruleBinary, generations, cells);

        // Determine the total width for the final padding
        int finalWidth = initialConditions.Length + 2 * generations;
        List<List<int>> paddedCa = PadImageData(ca, finalWidth);

        string imageData = $"P1\n{finalWidth} {generations}\n";
        foreach (var row in paddedCa)
        {
            imageData += string.Join("", row) + "\n";
        }

        File.WriteAllText($"results/r{ruleNumber}_g{generations}_i{initialConditions}_csharp.pbm", imageData);
    }
}
