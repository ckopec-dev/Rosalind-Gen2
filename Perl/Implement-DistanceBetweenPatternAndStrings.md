# Rosalind Problem: Implement DistanceBetweenPatternAndStrings

## Problem Description
The task is to find the distance between a pattern and a collection of strings. The distance is defined as the sum of the minimum Hamming distances from the pattern to each string in the collection.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to calculate Hamming distance between two strings
sub hamming_distance {
    my ($str1, $str2) = @_;
    my $distance = 0;
    
    for my $i (0..length($str1)-1) {
        if (substr($str1, $i, 1) ne substr($str2, $i, 1)) {
            $distance++;
        }
    }
    
    return $distance;
}

# Function to find the minimum Hamming distance between pattern and all k-mers in a string
sub min_hamming_distance {
    my ($pattern, $text) = @_;
    my $k = length($pattern);
    my $min_distance = length($pattern); # Initialize with maximum possible distance
    
    # Check all k-mers in text
    for my $i (0..length($text)-$k) {
        my $kmer = substr($text, $i, $k);
        my $distance = hamming_distance($pattern, $kmer);
        if ($distance < $min_distance) {
            $min_distance = $distance;
        }
    }
    
    return $min_distance;
}

# Main function to calculate distance between pattern and collection of strings
sub distance_between_pattern_and_strings {
    my ($pattern, @strings) = @_;
    my $total_distance = 0;
    
    foreach my $string (@strings) {
        $total_distance += min_hamming_distance($pattern, $string);
    }
    
    return $total_distance;
}

# Read input from stdin
my $pattern = <STDIN>;
chomp $pattern;

my @strings;
while (my $line = <STDIN>) {
    chomp $line;
    push @strings, $line;
}

# Calculate and print the result
my $result = distance_between_pattern_and_strings($pattern, @strings);
print "$result\n";
```

## Example Usage

If the input is:
```
AAA
TTACCTTAAC
GATATCTGAC
AAAGATCTGA
```

The program will:
1. Calculate the minimum Hamming distance from "AAA" to each string
2. Sum these distances to get the final result

## Explanation of Key Functions

- **hamming_distance**: Calculates the Hamming distance between two strings of equal length
- **min_hamming_distance**: Finds the minimum Hamming distance between the pattern and all possible k-mers in a given string
- **distance_between_pattern_and_strings**: Sums up the minimum distances for all strings in the collection

## Time Complexity
- O(n × m × k) where n is the number of strings, m is the average length of strings, and k is the length of the pattern

## Space Complexity
- O(1) additional space (excluding input storage)

