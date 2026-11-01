#!/usr/bin/perl
use strict;
use warnings;

sub die_msg {
    print STDERR "$_[0]\n";
    exit 1;
}

sub parse_int {
    my ($text) = @_;
    $text =~ s/^\s+|\s+$//g;
    my $sign = 1;
    my $digits = $text;
    if ($text =~ /^-/) {
        $sign = -1;
        $digits = substr($text, 1);
    }
    return undef if $digits eq '' || $digits !~ /^\d+$/;
    return undef if $digits > 2_147_483_647;
    return $sign * int($digits);
}

sub valid_seed {
    my ($seed) = @_;
    return defined $seed && $seed ne '' && $seed =~ /^[01]+$/;
}

open my $input, '<', 'input.txt' or die_msg('cannot read input.txt');
my $rule_line = <$input>;
my $seed = <$input>;
my $gen_line = <$input>;
close $input;
die_msg('invalid input') if !defined $rule_line || !defined $seed || !defined $gen_line;
chomp($rule_line, $seed, $gen_line);
$seed =~ s/\r$//;

my $rule = parse_int($rule_line);
die_msg('invalid rule') if !defined $rule || $rule < 0 || $rule > 255;
die_msg('invalid initial conditions') if !valid_seed($seed);
my $generations = parse_int($gen_line);
die_msg('invalid generations') if !defined $generations || $generations < 1;

my $n = length($seed);
die_msg('invalid generations') if $generations > int((2_147_483_647 - $n) / 2);
my $width = $n + 2 * $generations;
my @rule = map { ($rule >> $_) & 1 } 0 .. 7;
my $even = ($rule & 1) == 0;
my @prev = (0) x $width;
my @next = (0) x $width;
for my $i (0 .. $n - 1) {
    $prev[$generations + $i] = 1 if substr($seed, $i, 1) eq '1';
}

mkdir 'results' unless -d 'results';
my $path = "results/r${rule}_g${generations}_i${seed}_perl.pbm";
open my $out, '>', $path or die_msg('cannot write output');
binmode $out;
print $out "P1\n$width $generations\n";

for my $gen (0 .. $generations - 1) {
    print $out map { $_ ? '1' : '0' } @prev;
    print $out "\n";
    last if $gen + 1 == $generations;

    @next = (0) x $width;
    my ($lo, $hi) = $even
        ? ($generations - ($gen + 1), $generations + $n + ($gen + 1))
        : (0, $width);
    for my $j ($lo .. $hi - 1) {
        my $left = $j > 0 ? $prev[$j - 1] : 0;
        my $right = $j + 1 < $width ? $prev[$j + 1] : 0;
        my $idx = ($left << 2) | ($prev[$j] << 1) | $right;
        $next[$j] = $rule[$idx];
    }
    @prev = @next;
}

close $out;
