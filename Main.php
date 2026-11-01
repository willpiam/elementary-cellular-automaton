<?php

function die_msg($msg) {
    fwrite(STDERR, $msg . "\n");
    exit(1);
}

function parse_int_strict($text) {
    $t = trim($text);
    $sign = 1;
    $digits = $t;
    if (strlen($t) > 0 && $t[0] === '-') {
        $sign = -1;
        $digits = substr($t, 1);
    }
    if ($digits === '' || !ctype_digit($digits)) {
        return null;
    }
    $value = intval($digits, 10);
    if ((string)$value !== ltrim($digits, '0') && !( $value === 0 && preg_match('/^0+$/', $digits))) {
        return null;
    }
    return $sign * $value;
}

function valid_seed($seed) {
    return $seed !== '' && preg_match('/^[01]+$/', $seed) === 1;
}

$handle = fopen('input.txt', 'r');
if ($handle === false) {
    die_msg('cannot read input.txt');
}
$ruleLine = fgets($handle);
$seed = fgets($handle);
$genLine = fgets($handle);
fclose($handle);
if ($ruleLine === false || $seed === false || $genLine === false) {
    die_msg('invalid input');
}
$seed = rtrim($seed, "\r\n");
$rule = parse_int_strict($ruleLine);
$generations = parse_int_strict($genLine);
if ($rule === null || $rule < 0 || $rule > 255) {
    die_msg('invalid rule');
}
if (!valid_seed($seed)) {
    die_msg('invalid initial conditions');
}
if ($generations === null || $generations < 1) {
    die_msg('invalid generations');
}

$n = strlen($seed);
if ($generations > intdiv(PHP_INT_MAX - $n, 2)) {
    die_msg('invalid generations');
}
$width = $n + 2 * $generations;
$ruleTable = [];
for ($i = 0; $i < 8; $i++) {
    $ruleTable[$i] = ($rule >> $i) & 1;
}
$even = ($rule & 1) === 0;
$prev = array_fill(0, $width, 0);
$next = array_fill(0, $width, 0);
for ($i = 0; $i < $n; $i++) {
    if ($seed[$i] === '1') {
        $prev[$generations + $i] = 1;
    }
}

if (!is_dir('results') && !mkdir('results', 0755, true)) {
    die_msg('cannot create results');
}
$path = "results/r{$rule}_g{$generations}_i{$seed}_php.pbm";
$out = fopen($path, 'wb');
if ($out === false) {
    die_msg('cannot write output');
}
fwrite($out, "P1\n{$width} {$generations}\n");
for ($gen = 0; $gen < $generations; $gen++) {
    $line = '';
    for ($j = 0; $j < $width; $j++) {
        $line .= $prev[$j] === 0 ? '0' : '1';
    }
    fwrite($out, $line . "\n");
    if ($gen + 1 === $generations) {
        break;
    }
    for ($j = 0; $j < $width; $j++) {
        $next[$j] = 0;
    }
    if ($even) {
        $lo = $generations - ($gen + 1);
        $hi = $generations + $n + ($gen + 1);
    } else {
        $lo = 0;
        $hi = $width;
    }
    for ($j = $lo; $j < $hi; $j++) {
        $left = $j > 0 ? $prev[$j - 1] : 0;
        $right = $j + 1 < $width ? $prev[$j + 1] : 0;
        $idx = ($left << 2) | ($prev[$j] << 1) | $right;
        $next[$j] = $ruleTable[$idx];
    }
    $tmp = $prev;
    $prev = $next;
    $next = $tmp;
}
fclose($out);
