# Rosalind Problem: Implement Additive Phylogeny in Perl

## Problem Description
The Additive Phylogeny problem involves constructing a phylogenetic tree from a distance matrix that satisfies the additive property (the sum of edge weights along the unique path between any two leaves equals the distance between those leaves).

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse distance matrix
my @distances;
my $n = 0;

foreach my $line (@lines) {
    if ($line =~ /^\d+$/) {
        $n = $line;
    } else {
        my @row = split /\s+/, $line;
        push @distances, \@row;
    }
}

# Function to find the minimum distance between two leaves
sub find_min_distance {
    my ($matrix, $i, $j) = @_;
    return $matrix->[$i][$j];
}

# Function to find the pair of leaves that are closest to each other
sub find_closest_pair {
    my ($matrix, $n) = @_;
    my $min_dist = 999999;
    my ($i, $j) = (0, 0);
    
    for my $a (0..$n-1) {
        for my $b ($a+1..$n-1) {
            if ($matrix->[$a][$b] < $min_dist) {
                $min_dist = $matrix->[$a][$b];
                ($i, $j) = ($a, $b);
            }
        }
    }
    
    return ($i, $j);
}

# Function to compute the limb length for a leaf
sub compute_limb_length {
    my ($matrix, $leaf, $n) = @_;
    
    my $min_length = 999999;
    
    for my $i (0..$n-1) {
        next if $i == $leaf;
        for my $j (0..$n-1) {
            next if $j == $leaf || $j == $i;
            my $length = ($matrix->[$leaf][$i] + $matrix->[$leaf][$j] - $matrix->[$i][$j]) / 2;
            if ($length < $min_length) {
                $min_length = $length;
            }
        }
    }
    
    return $min_length;
}

# Function to remove a leaf from the distance matrix
sub remove_leaf {
    my ($matrix, $leaf, $n) = @_;
    my @new_matrix;
    
    for my $i (0..$n-1) {
        next if $i == $leaf;
        my @row;
        for my $j (0..$n-1) {
            next if $j == $leaf;
            push @row, $matrix->[$i][$j];
        }
        push @new_matrix, \@row;
    }
    
    return \@new_matrix;
}

# Function to add a new internal node
sub add_internal_node {
    my ($tree, $node1, $node2, $weight) = @_;
    push @$tree, [$node1, $node2, $weight];
    return $tree;
}

# Function to build the tree recursively
sub additive_phylogeny {
    my ($matrix, $n, $leaves, $tree) = @_;
    
    if ($n == 2) {
        # Base case: two leaves
        push @$tree, [$leaves->[0], $leaves->[1], $matrix->[0][1]];
        return $tree;
    }
    
    # Find the limb length for the last leaf
    my $last_leaf = $leaves->[$n-1];
    my $limb_length = compute_limb_length($matrix, $last_leaf, $n);
    
    # Adjust the distance matrix
    for my $i (0..$n-2) {
        $matrix->[$i][$n-1] = $matrix->[$i][$n-1] - $limb_length;
        $matrix->[$n-1][$i] = $matrix->[$i][$n-1];
    }
    
    # Find the closest pair among the remaining leaves
    my ($i, $j) = find_closest_pair($matrix, $n-1);
    
    # Compute the distance from the last leaf to the path between i and j
    my $x = ($matrix->[$i][$n-1] + $matrix->[$j][$n-1] - $matrix->[$i][$j]) / 2;
    my $y = $matrix->[$i][$n-1] - $x;
    
    # Create a new leaf list without the last leaf
    my @new_leaves = @$leaves;
    pop @new_leaves;
    
    # Recursively build the tree for the smaller matrix
    my $new_matrix = remove_leaf($matrix, $n-1, $n);
    additive_phylogeny($new_matrix, $n-1, \@new_leaves, $tree);
    
    # Add the new leaf and its connections
    my $new_node = $n + 100; # Unique node identifier
    push @$tree, [$last_leaf, $new_node, $limb_length];
    
    # Add the connection between the new node and the existing tree
    push @$tree, [$new_node, $i, $x];
    push @$tree, [$new_node, $j, $y];
    
    return $tree;
}

# Main execution
my @leaves = (0..$n-1);
my @tree;

# Build the tree
my $result = additive_phylogeny(\@distances, $n, \@leaves, \@tree);

# Output the tree edges
foreach my $edge (@$result) {
    print join("\t", @$edge), "\n";
}
```

## Explanation

This implementation follows the additive phylogeny algorithm:

1. **Base Case**: If there are only 2 leaves, directly connect them with the distance between them.

2. **Recursive Case**: 
   - For the current set of leaves, compute the limb length for the last leaf
   - Adjust the distance matrix by subtracting the limb length
   - Find the closest pair among the remaining leaves
   - Compute the position of the new leaf along the path between the closest pair
   - Recursively build the tree for the smaller matrix
   - Add the new leaf and its connections to the tree

3. **Key Functions**:
   - `compute_limb_length`: Calculates the limb length using the formula for the distance from a leaf to the tree
   - `find_closest_pair`: Finds the pair of leaves with minimum distance
   - `remove_leaf`: Removes a leaf from the distance matrix
   - `additive_phylogeny`: Main recursive function that builds the tree

## Input Format
The input should be a distance matrix where:
- First line contains the number of leaves (n)
- Next n lines contain n space-separated integers representing the distance matrix

## Output Format
The output consists of edges in the format: `node1 node2 weight` representing the phylogenetic tree.

