

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;

public class Main {

    public static List<Integer> ruleToBinaryArray(int ruleNumber) {
        String binaryString = String.format("%8s", Integer.toBinaryString(ruleNumber)).replace(' ', '0');
        List<Integer> binaryArray = new ArrayList<>();
        for (char bit : binaryString.toCharArray()) {
            binaryArray.add(Character.getNumericValue(bit));
        }
        return binaryArray;
    }

    public static int calculateCell(String pState, List<Integer> rule) {
        HashMap<String, Integer> ruleMap = new HashMap<>() {{
            put("111", rule.get(0));
            put("110", rule.get(1));
            put("101", rule.get(2));
            put("100", rule.get(3));
            put("011", rule.get(4));
            put("010", rule.get(5));
            put("001", rule.get(6));
            put("000", rule.get(7));
        }};
        return ruleMap.get(pState);
    }


    public static List<List<Integer>> runCellularAutomaton(List<Integer> rule, int generations, List<Integer> initialCells) {
        List<Integer> cells = new ArrayList<>(initialCells);
        List<List<Integer>> ca = new ArrayList<>();

        for (int i = 0; i < generations - 1; i++) {
            List<Integer> extendedCells = new ArrayList<>();
            extendedCells.add(0);
            extendedCells.add(0);
            extendedCells.addAll(cells); // Add all cells to the extended list here
            extendedCells.add(0);
            extendedCells.add(0);
        
            ca.add(new ArrayList<>(cells));

            List<Integer> nextGeneration = new ArrayList<>();
            for (int j = 1; j < extendedCells.size() - 1; j++) {
                String neighborhood = "" + extendedCells.get(j - 1) + extendedCells.get(j) + extendedCells.get(j + 1);
                nextGeneration.add(calculateCell(neighborhood, rule));
            }
            cells = nextGeneration;
        }

        ca.add(cells);
        return ca;
    }


    public static List<List<Integer>> padImageData(List<List<Integer>> imageData, int totalWidth) {
        List<List<Integer>> paddedData = new ArrayList<>();
        for (List<Integer> row : imageData) {
            int paddingLength = (totalWidth - row.size()) / 2;
            List<Integer> paddedRow = new ArrayList<>();
            for (int i = 0; i < paddingLength; i++) {
                paddedRow.add(0);
            }
            paddedRow.addAll(row);
            for (int i = 0; i < paddingLength; i++) {
                paddedRow.add(0);
            }
            paddedData.add(paddedRow);
        }
        return paddedData;
    }

    public static void readInputsFromFile(String filePath) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get(filePath));
        int ruleNumber = Integer.parseInt(lines.get(0).trim());
        String initialConditions = lines.get(1).trim();
        int generations = Integer.parseInt(lines.get(2).trim());

        List<Integer> ruleBinary = ruleToBinaryArray(ruleNumber);
        List<Integer> cells = new ArrayList<>();
        for (char bit : initialConditions.toCharArray()) {
            cells.add(Character.getNumericValue(bit));
        }

        List<List<Integer>> ca = runCellularAutomaton(ruleBinary, generations, cells);

        // Determine the total width for the final padding
        int finalWidth = initialConditions.length() + 2 * generations;
        List<List<Integer>> paddedCa = padImageData(ca, finalWidth);

        StringBuilder imageData = new StringBuilder("P1\n" + finalWidth + " " + generations + "\n");
        for (List<Integer> row : paddedCa) {
            for (int num : row) {
                imageData.append(num);
            }
            imageData.append("\n");
        }

        Files.write(Paths.get("results/r" + ruleNumber + "_g" + generations + "_i" + initialConditions + "_java.pbm"), imageData.toString().getBytes());
    }

    public static void main(String[] args) {
        try {
            readInputsFromFile("input.txt");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
