import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.ArrayList;
import java.util.Collections; // Added import for Collections

public class Main {

    public static void main(String[] args) {
        long startTime = System.nanoTime();

        String[] inputs = readInputsFromFile("input.txt");
        int ruleNumber = Integer.parseInt(inputs[0]);
        String initialConditions = inputs[1];
        int generations = Integer.parseInt(inputs[2]);

        System.out.println("Rule Number: " + ruleNumber);
        System.out.println("Initial Conditions: " + initialConditions);
        System.out.println("Generations: " + generations);

        runCellularAutomaton(ruleNumber, generations, initialConditions);

        long endTime = System.nanoTime();
        System.out.printf("Took %.2fs to generate %d generations of rule %d%n", (endTime - startTime) / 1e9, generations, ruleNumber);
    }

    public static void runCellularAutomaton(int ruleNumber, int generations, String initialConditions) {
        List<Integer> cells = initialConditions.chars()
                .mapToObj(c -> Integer.parseInt(String.valueOf((char) c)))
                .collect(Collectors.toList());

        List<Integer> ruleBinary = ruleToBinaryArray(ruleNumber);

        int imageWidth = cells.size() + 2 * generations;
        StringBuilder imageData = new StringBuilder("P1\n" + imageWidth + " " + generations + "\n");

        for (int i = 0; i < generations; i++) {
            int paddingLength = (imageWidth - cells.size()) / 2;
            List<Integer> padding = Collections.nCopies(paddingLength, 0); // Changed to Collections.nCopies

            List<Integer> extendedCells = Stream.concat(Stream.concat(padding.stream(), cells.stream()), padding.stream())
                    .collect(Collectors.toList());

            imageData.append(extendedCells.stream()
                    .map(cell -> cell != 0 ? "1" : "0")
                    .collect(Collectors.joining()))
                    .append("\n");

            List<Integer> nextGeneration = new ArrayList<>();
            for (int j = 1; j < extendedCells.size() - 1; j++) {
                String neighborhood = "" + extendedCells.get(j - 1) + extendedCells.get(j) + extendedCells.get(j + 1);
                nextGeneration.add(calculateCell(neighborhood, ruleBinary));
            }
            cells = nextGeneration;
        }

        try (BufferedWriter file = new BufferedWriter(new FileWriter("results/r" + ruleNumber + "_g" + generations + "_i" + initialConditions + "_java.pbm"))) {
            file.write(imageData.toString());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static List<Integer> ruleToBinaryArray(int ruleNumber) {
        return String.format("%8s", Integer.toBinaryString(ruleNumber)).replace(' ', '0')
                .chars()
                .mapToObj(c -> Integer.parseInt(String.valueOf((char) c)))
                .collect(Collectors.toList());
    }

    public static int calculateCell(String pState, List<Integer> rule) {
        HashMap<String, Integer> ruleMap = new HashMap<>();
        ruleMap.put("111", rule.get(0));
        ruleMap.put("110", rule.get(1));
        ruleMap.put("101", rule.get(2));
        ruleMap.put("100", rule.get(3));
        ruleMap.put("011", rule.get(4));
        ruleMap.put("010", rule.get(5));
        ruleMap.put("001", rule.get(6));
        ruleMap.put("000", rule.get(7));

        return ruleMap.get(pState);
    }

    public static String[] readInputsFromFile(String filePath) {
        try {
            List<String> lines = Files.readAllLines(Paths.get(filePath));
            return new String[]{lines.get(0).trim(), lines.get(1).trim(), lines.get(2).trim()};
        } catch (IOException e) {
            e.printStackTrace();
            return new String[]{"0", "", "0"};
        }
    }
}
