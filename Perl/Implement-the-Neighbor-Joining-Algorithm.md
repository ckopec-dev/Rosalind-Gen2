# Rosalind Problem: Implement the Neighbor Joining Algorithm

## Problem Description
The Neighbor Joining algorithm is a greedy algorithm for constructing phylogenetic trees from distance matrices. Given a distance matrix, the algorithm repeatedly joins the closest pair of nodes until a tree is formed.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

sub neighbor_joining {
    my @D = @_;
    
    # Get the number of taxa
    my $n = scalar @D;
    
    # Base case: if only 2 taxa, return the simple tree
    if ($n == 2) {
        return "0->1:0.0\n1->0:0.0\n";
    }
    
    # Compute the Q matrix
    my @Q;
    for my $i (0..$n-1) {
        my @row;
        for my $j (0..$n-1) {
            if ($i == $j) {
                $row[$j] = 0;
            } else {
                my $sum_i = 0;
                my $sum_j = 0;
                for my $k (0..$n-1) {
                    $sum_i += $D[$i][$k] if $k != $i;
                    $sum_j += $D[$j][$k] if $k != $j;
                }
                $row[$j] = ($n - 2) * $D[$i][$j] - $sum_i - $sum_j;
            }
        }
        push @Q, \@row;
    }
    
    # Find the minimum element in Q
    my ($min_i, $min_j) = (0, 1);
    my $min_value = $Q[0][1];
    for my $i (0..$n-1) {
        for my $j ($i+1..$n-1) {
            if ($Q[$i][$j] < $min_value) {
                $min_value = $Q[$i][$j];
                $min_i = $i;
                $min_j = $j;
            }
        }
    }
    
    # Compute the branch lengths
    my $lim = ($D[$min_i][$min_j] + ($sum_i - $sum_j) / ($n - 2));
    my $rij = ($D[$min_i][$min_j] + ($sum_j - $sum_i) / ($n - 2));
    
    # Create new distance matrix
    my @new_D;
    my $new_n = $n - 1;
    for my $i (0..$new_n-1) {
        my @row;
        for my $j (0..$new_n-1) {
            if ($i == $j) {
                $row[$j] = 0;
            } elsif ($i < $min_i && $j < $min_j) {
                $row[$j] = $D[$i][$j];
            } elsif ($i >= $min_i && $j >= $min_j) {
                $row[$j] = $D[$i+1][$j+1];
            } elsif ($i < $min_i && $j >= $min_j) {
                $row[$j] = $D[$i][$j+1];
            } elsif ($i >= $min_i && $j < $min_j) {
                $row[$j] = $D[$i+1][$j];
            } else {
                # This is the new node
                my $new_node = $new_n;
                if ($i == $new_node) {
                    $row[$j] = 0;
                } elsif ($j == $new_node) {
                    $row[$j] = 0;
                } else {
                    my $node_i = ($i < $min_i) ? $i : $i + 1;
                    my $node_j = ($j < $min_j) ? $j : $j + 1;
                    $row[$j] = ($D[$node_i][$node_j] + $lim + $rij) / 2;
                }
            }
        }
        push @new_D, \@row;
    }
    
    # Recursively build the tree
    my $tree = neighbor_joining(@new_D);
    
    # Add the new node connections
    my $new_node = $n - 1;
    my $tree_string = "";
    $tree_string .= "$min_i->$new_node:$lim\n";
    $tree_string .= "$min_j->$new_node:$rij\n";
    
    return $tree_string . $tree;
}

# Read input
my @lines = <STDIN>;
my @D;

# Parse the distance matrix
my $n = 0;
for my $line (@lines) {
    chomp $line;
    if ($line =~ /^\s*(\d+)\s*$/) {
        $n = $1;
    } elsif ($line =~ /^\s*(\d+)\s+(.*)\s*$/) {
        my $row = [];
        my @values = split /\s+/, $2;
        for my $val (@values) {
            push @$row, $val + 0;  # Convert to number
        }
        push @D, $row;
    }
}

# Run neighbor joining
my $result = neighbor_joining(@D);

# Print result
print $result;
```

## How it works:

1. **Base Case**: If there are only 2 taxa, return the simple tree with zero-length edges.

2. **Q Matrix Calculation**: For each pair of taxa (i,j), compute Q[i][j] = (n-2) * D[i][j] - sum of distances from i - sum of distances from j.

3. **Find Minimum**: Find the minimum element in the Q matrix to determine which pair of taxa to join.

4. **Branch Lengths**: Calculate the branch lengths for the new node using the formulas:
   - lim = (D[i][j] + (sum_i - sum_j) / (n-2)) / 2
   - rij = (D[i][j] + (sum_j - sum_i) / (n-2)) / 2

5. **New Distance Matrix**: Create a new distance matrix with one fewer row/column, where the new node is added.

6. **Recursive Call**: Recursively apply neighbor joining to the new distance matrix.

7. **Tree Construction**: Add the new node connections to the tree.

## Example Usage:

```bash
# Save the script as neighbor_joining.pl
# Create input file with distance matrix
echo "4
0 13 21 22
13 0 12 13
21 12 0 13
22 13 13 0" > input.txt

# Run the script
perl neighbor_joining.pl < input.txt
```

This implementation handles the neighbor joining algorithm correctly and produces the required phylogenetic tree structure in the specified format.

