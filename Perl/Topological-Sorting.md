# Rosalind Problem: Topological Sorting

## Problem Description
Given a directed acyclic graph (DAG), return a topological ordering of its vertices.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse the graph
my %in_degree;
my %adj_list;

# Process each edge
foreach my $line (@lines) {
    next if $line =~ /^\s*$/;  # Skip empty lines
    
    my ($from, $to) = split /\s+/, $line;
    
    # Initialize in-degrees
    $in_degree{$from} //= 0;
    $in_degree{$to} //= 0;
    
    # Build adjacency list
    push @{$adj_list{$from}}, $to;
    
    # Increment in-degree of destination
    $in_degree{$to}++;
}

# Find all nodes with in-degree 0
my @sources;
foreach my $node (keys %in_degree) {
    push @sources, $node if $in_degree{$node} == 0;
}

# Topological sort using Kahn's algorithm
my @topological_order;

while (@sources) {
    # Remove a node with in-degree 0
    my $current = pop @sources;
    push @topological_order, $current;
    
    # For each neighbor of current node
    if (exists $adj_list{$current}) {
        foreach my $neighbor (@{$adj_list{$current}}) {
            # Reduce in-degree
            $in_degree{$neighbor}--;
            
            # If in-degree becomes 0, add to sources
            if ($in_degree{$neighbor} == 0) {
                push @sources, $neighbor;
            }
        }
    }
}

# Output result
print join(" ", @topological_order) . "\n";
```

## How it works

1. **Parse Input**: Read edges from stdin and build adjacency list representation
2. **Calculate In-degrees**: Count incoming edges for each node
3. **Find Sources**: Identify nodes with in-degree 0 (no incoming edges)
4. **Kahn's Algorithm**: 
   - Remove nodes with in-degree 0
   - Add them to result
   - Reduce in-degrees of their neighbors
   - Add neighbors with in-degree 0 to sources
5. **Output**: Print the topological ordering

## Example Usage

Input:
```
5 4
4 2
4 3
2 1
3 1
```

Output:
```
5 4 2 3 1
```

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for adjacency list and in-degree tracking

This solution implements Kahn's algorithm for topological sorting, which is efficient and handles the constraints of the Rosalind problem correctly.

