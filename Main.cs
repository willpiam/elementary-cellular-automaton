using System;
using System.IO;
using System.Text;

class CellularAutomaton
{
    static void Die(string msg)
    {
        Console.Error.WriteLine(msg);
        Environment.Exit(1);
    }

    static bool ParseInt(string text, out int value)
    {
        value = 0;
        string t = text.Trim().TrimEnd('\r');
        if (t.Length == 0)
            return false;
        bool neg = false;
        string digits = t;
        if (t[0] == '-')
        {
            neg = true;
            digits = t.Substring(1);
        }
        if (digits.Length == 0)
            return false;
        foreach (char c in digits)
        {
            if (c < '0' || c > '9')
                return false;
        }
        if (!int.TryParse(digits, out value))
            return false;
        if (neg)
            value = -value;
        return true;
    }

    static bool ValidSeed(string seed)
    {
        if (string.IsNullOrEmpty(seed))
            return false;
        foreach (char c in seed)
        {
            if (c != '0' && c != '1')
                return false;
        }
        return true;
    }

    static void Main()
    {
        string[] lines;
        try
        {
            lines = File.ReadAllLines("input.txt");
        }
        catch (Exception)
        {
            Die("cannot read input.txt");
            return;
        }
        if (lines.Length < 3)
            Die("invalid input");

        int ruleNumber;
        if (!ParseInt(lines[0], out ruleNumber) || ruleNumber < 0 || ruleNumber > 255)
            Die("invalid rule");
        string seed = lines[1].TrimEnd('\r');
        if (!ValidSeed(seed))
            Die("invalid initial conditions");
        int generations;
        if (!ParseInt(lines[2], out generations) || generations < 1)
            Die("invalid generations");

        int n = seed.Length;
        if (generations > (int.MaxValue - n) / 2)
            Die("invalid generations");
        int width = n + 2 * generations;
        bool even = (ruleNumber & 1) == 0;
        byte[] rule = new byte[8];
        for (int i = 0; i < 8; i++)
            rule[i] = (byte)((ruleNumber >> i) & 1);

        byte[] prev = new byte[width];
        byte[] next = new byte[width];
        for (int i = 0; i < n; i++)
        {
            if (seed[i] == '1')
                prev[generations + i] = 1;
        }

        Directory.CreateDirectory("results");
        string path = $"results/r{ruleNumber}_g{generations}_i{seed}_csharp.pbm";
        using (FileStream fs = new FileStream(path, FileMode.Create, FileAccess.Write))
        {
            byte[] header = Encoding.ASCII.GetBytes($"P1\n{width} {generations}\n");
            fs.Write(header, 0, header.Length);
            byte[] line = new byte[width + 1];
            line[width] = (byte)'\n';

            for (int gen = 0; gen < generations; gen++)
            {
                for (int j = 0; j < width; j++)
                    line[j] = prev[j] == 0 ? (byte)'0' : (byte)'1';
                fs.Write(line, 0, line.Length);
                if (gen + 1 == generations)
                    break;

                Array.Clear(next, 0, width);
                int lo = even ? generations - (gen + 1) : 0;
                int hi = even ? generations + n + (gen + 1) : width;
                for (int j = lo; j < hi; j++)
                {
                    int left = j > 0 ? prev[j - 1] : 0;
                    int right = j + 1 < width ? prev[j + 1] : 0;
                    int idx = (left << 2) | (prev[j] << 1) | right;
                    next[j] = rule[idx];
                }
                byte[] tmp = prev;
                prev = next;
                next = tmp;
            }
        }
    }
}
