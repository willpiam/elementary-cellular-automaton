#!/usr/bin/perl

#!/usr/bin/perl

use strict;
use warnings;

sub rule_to_binary_array {
    my ($rule_number) = @_;
    my $binary_string = sprintf("%08b", $rule_number);
    return split('', $binary_string);
}

sub calculate_cell {
    my ($p_state, $rule) = @_;
    my %rule_map = (
        "111" => $rule->[0],
        "110" => $rule->[1],
        "101" => $rule->[2],
        "100" => $rule->[3],
        "011" => $rule->[4],
        "010" => $rule->[5],
        "001" => $rule->[6],
        "000" => $rule->[7],
    );
    return $rule_map{$p_state};
}

sub run_cellular_automaton {
    my ($rule, $generations, $initial_cells) = @_;
    my @cells = @$initial_cells;
    my @ca;

    for my $gen (1 .. $generations) {
        push @ca, [@cells];

        my @next_generation;
        my @extended_cells = (0, 0, @cells, 0, 0);
        for my $j (1 .. $#extended_cells - 1) {
            my $neighborhood = join('', @extended_cells[$j - 1 .. $j + 1]);
            push @next_generation, calculate_cell($neighborhood, $rule);
        }
        @cells = @next_generation;
    }
    
    return \@ca;
}

sub pad_image_data {
    my ($image_data, $total_width) = @_;
    my @padded_data;

    for my $row (@$image_data) {
        my $padding_length = int(($total_width - scalar @$row) / 2);
        my @padding = ((0) x $padding_length);
        push @padded_data, [@padding, @$row, @padding];
    }
    
    return \@padded_data;
}

sub read_inputs_from_file {
    my ($file_path) = @_;
    open my $file, '<', $file_path or die "Could not open file: $!";
    chomp(my $rule_number = <$file>);
    chomp(my $initial_conditions = <$file>);
    chomp(my $generations = <$file>);
    close $file;

    return ($rule_number, $initial_conditions, $generations);
}

sub main {
    my ($rule_number, $initial_conditions, $generations) = read_inputs_from_file('input.txt');

    my @rule_binary = rule_to_binary_array($rule_number);
    my @cells = split('', $initial_conditions);

    my $ca = run_cellular_automaton(\@rule_binary, $generations, \@cells);

    my $final_width = length($initial_conditions) + 2 * $generations;
    my $padded_ca = pad_image_data($ca, $final_width);

    my $image_data = "P1\n$final_width $generations\n";
    for my $row (@$padded_ca) {
        $image_data .= join('', @$row) . "\n";
    }

    open my $file, '>', "results/r${rule_number}_g${generations}_i${initial_conditions}_perl.pbm" or die "Could not open file: $!";
    print $file $image_data;
    close $file;
}

main();
