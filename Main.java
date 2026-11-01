import java.io.BufferedOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class Main {
    static void die(String msg) {
        System.err.println(msg);
        System.exit(1);
    }

    static Integer parseIntStrict(String text) {
        String t = text.trim();
        if (t.endsWith("\r"))
            t = t.substring(0, t.length() - 1).trim();
        if (t.isEmpty())
            return null;
        int sign = 1;
        int i = 0;
        if (t.charAt(0) == '-') {
            sign = -1;
            i = 1;
        }
        if (i >= t.length())
            return null;
        long value = 0;
        for (; i < t.length(); i++) {
            char c = t.charAt(i);
            if (c < '0' || c > '9')
                return null;
            value = value * 10 + (c - '0');
            if (value > Integer.MAX_VALUE)
                return null;
        }
        return (int) (sign * value);
    }

    static boolean validSeed(String seed) {
        if (seed.isEmpty())
            return false;
        for (int i = 0; i < seed.length(); i++) {
            char c = seed.charAt(i);
            if (c != '0' && c != '1')
                return false;
        }
        return true;
    }

    public static void main(String[] args) {
        List<String> lines;
        try {
            lines = Files.readAllLines(Paths.get("input.txt"));
        } catch (IOException e) {
            die("cannot read input.txt");
            return;
        }
        if (lines.size() < 3)
            die("invalid input");

        Integer ruleNumber = parseIntStrict(lines.get(0));
        if (ruleNumber == null || ruleNumber < 0 || ruleNumber > 255)
            die("invalid rule");
        String seed = lines.get(1);
        if (seed.endsWith("\r"))
            seed = seed.substring(0, seed.length() - 1);
        if (!validSeed(seed))
            die("invalid initial conditions");
        Integer generations = parseIntStrict(lines.get(2));
        if (generations == null || generations < 1)
            die("invalid generations");

        int n = seed.length();
        if (generations > (Integer.MAX_VALUE - n) / 2)
            die("invalid generations");
        int width = n + 2 * generations;
        boolean even = (ruleNumber & 1) == 0;
        byte[] rule = new byte[8];
        for (int i = 0; i < 8; i++)
            rule[i] = (byte) ((ruleNumber >> i) & 1);

        byte[] prev = new byte[width];
        byte[] next = new byte[width];
        for (int i = 0; i < n; i++) {
            if (seed.charAt(i) == '1')
                prev[generations + i] = 1;
        }

        try {
            Files.createDirectories(Paths.get("results"));
            String filename = String.format("results/r%d_g%d_i%s_java.pbm", ruleNumber, generations, seed);
            try (BufferedOutputStream out = new BufferedOutputStream(Files.newOutputStream(Path.of(filename)))) {
                out.write(String.format("P1\n%d %d\n", width, generations).getBytes(StandardCharsets.US_ASCII));
                byte[] line = new byte[width + 1];
                line[width] = '\n';
                for (int gen = 0; gen < generations; gen++) {
                    for (int j = 0; j < width; j++)
                        line[j] = prev[j] == 0 ? (byte) '0' : (byte) '1';
                    out.write(line);
                    if (gen + 1 == generations)
                        break;
                    Arrays.fill(next, (byte) 0);
                    int lo = even ? generations - (gen + 1) : 0;
                    int hi = even ? generations + n + (gen + 1) : width;
                    for (int j = lo; j < hi; j++) {
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
        } catch (IOException e) {
            die("cannot write output");
        }
    }
}
