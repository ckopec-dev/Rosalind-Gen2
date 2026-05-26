# Rosalind Problem: Implement Hierarchical Clustering

## Problem Description
Implement hierarchical clustering using the UPGMA (Unweighted Pair Group Method with Arithmetic Mean) algorithm.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

# Read input data
my @lines = <STDIN>;
chomp @lines;

# Parse distance matrix
my @matrix;
my @names;
my $n = 0;

# First line contains the number of taxa
my $num_taxa = $lines[0];
$n = $num_taxa;

# Parse the distance matrix
for my $i (1..$n) {
    my @row = split /\s+/, $lines[$i];
    push @names, $row[0];
    my @distances = @row[1..$#row];
    push @matrix, \@distances;
}

# Function to find minimum distance in matrix
sub find_min_distance {
    my ($dist_matrix, $clusters) = @_;
    my $min_dist = 999999;
    my $min_i = -1;
    my $min_j = -1;
    
    for my $i (0..$#{$dist_matrix}) {
        next if $clusters->[$i] == 0;  # Skip if cluster already merged
        for my $j ($i+1..$#{$dist_matrix}) {
            next if $clusters->[$j] == 0;  # Skip if cluster already merged
            if ($dist_matrix->[$i][$j] < $min_dist) {
                $min_dist = $dist_matrix->[$i][$j];
                $min_i = $i;
                $min_j = $j;
            }
        }
    }
    return ($min_dist, $min_i, $min_j);
}

# Function to update distance matrix after merging clusters
sub update_matrix {
    my ($dist_matrix, $clusters, $i, $j, $new_cluster_id) = @_;
    
    # Create new row for merged cluster
    my @new_row;
    for my $k (0..$#{$dist_matrix}) {
        next if $clusters->[$k] == 0;  # Skip if cluster already merged
        if ($k == $i || $k == $j) {
            push @new_row, 0;
        } else {
            my $dist1 = $dist_matrix->[$i][$k];
            my $dist2 = $dist_matrix->[$j][$k];
            my $avg_dist = ($dist1 + $dist2) / 2;
            push @new_row, $avg_dist;
        }
    }
    
    # Update matrix
    for my $k (0..$#{$dist_matrix}) {
        if ($k == $i || $k == $j) {
            $dist_matrix->[$k][$new_cluster_id] = 0;
            $dist_matrix->[$new_cluster_id][$k] = 0;
        } else {
            $dist_matrix->[$new_cluster_id][$k] = $new_row[$k];
            $dist_matrix->[$k][$new_cluster_id] = $new_row[$k];
        }
    }
    
    # Mark old clusters as merged
    $clusters->[$i] = 0;
    $clusters->[$j] = 0;
}

# Main hierarchical clustering algorithm
my @clusters = (1) x $n;  # 1 = active cluster, 0 = merged
my @cluster_names = @names;
my $cluster_count = $n;
my @output;

# Create initial cluster pairs
my @cluster_pairs;
for my $i (0..$n-1) {
    for my $j ($i+1..$n-1) {
        push @cluster_pairs, [$i, $j];
    }
}

# Perform UPGMA clustering
while ($cluster_count > 1) {
    # Find minimum distance
    my ($min_dist, $min_i, $min_j) = find_min_distance(\@matrix, \@clusters);
    
    # Create new cluster name
    my $new_cluster_name = "Cluster_" . ($n - $cluster_count + 1);
    my $cluster1_name = $cluster_names[$min_i];
    my $cluster2_name = $cluster_names[$min_j];
    
    # Output the merge
    my $output_line = sprintf("%.3f %s %s", $min_dist/2, $cluster1_name, $cluster2_name);
    push @output, $output_line;
    
    # Update cluster names
    $cluster_names[$min_i] = $cluster1_name . " " . $cluster2_name;
    $cluster_names[$min_j] = "";
    
    # Update matrix
    update_matrix(\@matrix, \@clusters, $min_i, $min_j, $min_i);
    
    # Update cluster count
    $cluster_count--;
}

# Print output
print join("\n", @output) . "\n";
```

## Explanation

This Perl implementation of hierarchical clustering using UPGMA follows these key steps:

1. **Input Parsing**: Reads the distance matrix from standard input
2. **Data Structure Setup**: Creates matrices and arrays to track clusters and distances
3. **Find Minimum Distance**: Identifies the smallest distance between any two active clusters
4. **Merge Clusters**: Combines the two closest clusters into a new cluster
5. **Update Distance Matrix**: Recalculates distances using average linkage method
6. **Output Results**: Prints the clustering steps with branch lengths

## Key Features

- **UPGMA Algorithm**: Uses unweighted pair group method with arithmetic mean
- **Average Linkage**: New distances calculated as average of old distances
- **Proper Output Format**: Shows branch lengths and cluster merges
- **Efficient Implementation**: Uses appropriate data structures for performance

## Usage

```bash
perl hierarchical_clustering.pl < input.txt
```

Where `input.txt` contains the distance matrix in the required format.

