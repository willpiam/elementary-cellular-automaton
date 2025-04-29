import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

public class Main {

    public static int[] ruleToBinaryArray(int ruleNumber) {
        int[] rule = new int[8];
        for (int i = 0; i < 8; i++) {
            rule[i] = (ruleNumber >> i) & 1;
        }
        return rule;
    }

    public static int[][] runCellularAutomaton(int[] rule, int generations, int[] initialCells) {
        int width = initialCells.length + 2 * generations;
        int[][] ca = new int[generations][width];
        int[] curr = new int[width];
        int[] next = new int[width];
        int offset = generations;
        System.arraycopy(initialCells, 0, curr, offset, initialCells.length);

        for (int gen = 0; gen < generations; gen++) {
            System.arraycopy(curr, 0, ca[gen], 0, width);
            for (int j = 1; j < width - 1; j++) {
                int idx = (curr[j - 1] << 2) | (curr[j] << 1) | curr[j + 1];
                next[j] = rule[idx];
            }
            next[0] = 0;
            next[width - 1] = 0;
            int[] temp = curr;
            curr = next;
            next = temp;
        }

        return ca;
    }

    public static void main(String[] args) {
        try {
            java.util.List<String> lines = Files.readAllLines(Paths.get("input.txt"));
            int ruleNumber = Integer.parseInt(lines.get(0).trim());
            String initial = lines.get(1).trim();
            int generations = Integer.parseInt(lines.get(2).trim());

            int[] rule = ruleToBinaryArray(ruleNumber);
            int[] initialCells = new int[initial.length()];
            for (int i = 0; i < initial.length(); i++) {
                initialCells[i] = initial.charAt(i) == '1' ? 1 : 0;
            }

            int[][] ca = runCellularAutomaton(rule, generations, initialCells);
            int width = initialCells.length + 2 * generations;

            Files.createDirectories(Paths.get("results"));
            String filename = String.format("results/r%d_g%d_i%s_java.pbm", ruleNumber, generations, initial);
            try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(filename),
                    StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)) {
                writer.write("P1\n");
                writer.write(width + " " + generations + "\n");
                char[] row = new char[width + 1];
                for (int gen = 0; gen < generations; gen++) {
                    for (int j = 0; j < width; j++) {
                        row[j] = ca[gen][j] == 1 ? '1' : '0';
                    }
                    row[width] = '\n';
                    writer.write(row);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
