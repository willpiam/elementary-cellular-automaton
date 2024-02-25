<?php

function ruleToBinaryArray($ruleNumber) {
    $binaryString = str_pad(decbin($ruleNumber), 8, "0", STR_PAD_LEFT);
    return array_map('intval', str_split($binaryString));
}

function calculateCell($pState, $rule) {
    $ruleMap = [
        "111" => $rule[0],
        "110" => $rule[1],
        "101" => $rule[2],
        "100" => $rule[3],
        "011" => $rule[4],
        "010" => $rule[5],
        "001" => $rule[6],
        "000" => $rule[7]
    ];
    return $ruleMap[$pState];
}

function runCellularAutomaton($rule, $generations, $initialCells) {
    $cells = $initialCells;
    $ca = [];

    for ($i = 0; $i < $generations - 1; $i++) {
        $extendedCells = array_merge([0, 0], $cells, [0, 0]);
        array_push($ca, $cells);

        $nextGeneration = [];
        for ($j = 1; $j < count($extendedCells) - 1; $j++) {
            $neighborhood = implode('', array_slice($extendedCells, $j - 1, 3));
            array_push($nextGeneration, calculateCell($neighborhood, $rule));
        }
        $cells = $nextGeneration;
    }

    array_push($ca, $cells);
    return $ca;
}

function padImageData($imageData, $totalWidth) {
    $paddedData = [];
    foreach ($imageData as $row) {
        $paddingLength = intval(($totalWidth - count($row)) / 2);
        $padding = array_fill(0, $paddingLength, 0);
        $paddedRow = array_merge($padding, $row, $padding);
        array_push($paddedData, $paddedRow);
    }
    return $paddedData;
}

function readInputsFromFile($filePath) {
    $file = fopen($filePath, 'r');
    $ruleNumber = intval(trim(fgets($file)));
    $initialConditions = str_split(trim(fgets($file)));
    $generations = intval(trim(fgets($file)));
    fclose($file);
    return array($ruleNumber, $initialConditions, $generations);
}

function main() {
    list($ruleNumber, $initialConditions, $generations) = readInputsFromFile('input.txt');

    $ruleBinary = ruleToBinaryArray($ruleNumber);
    $cells = array_map('intval', $initialConditions);

    $ca = runCellularAutomaton($ruleBinary, $generations, $cells);

    $finalWidth = count($initialConditions) + 2 * $generations;
    $paddedCa = padImageData($ca, $finalWidth);

    $imageData = "P1\n$finalWidth $generations\n";
    foreach ($paddedCa as $row) {
        $imageData .= implode('', $row) . "\n";
    }

    file_put_contents("results/r{$ruleNumber}_g{$generations}_i" . implode('', $initialConditions) . "_php.pbm", $imageData);
}

main();

?>
