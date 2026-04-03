# Rosalind Problem: Breadth-First Search (BFS)

## Problem Description
Given a directed graph with n nodes and m edges, find the shortest path from node 1 to all other nodes using breadth-first search.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse the graph
my ($n, $m) = split ' ', $lines[0];
my %graph;

# Build adjacency list representation
for my $i (1..$m) {
    my ($u, $v) = split ' ', $lines[$i];
    push @{$graph{$u}}, $v;
}

# BFS implementation
sub bfs {
    my ($start, $n) = @_;
    
    # Initialize distances array with -1 (unvisited)
    my @dist = (-1) x ($n + 1);
    $dist[$start] = 0;
    
    # Initialize queue with start node
    my @queue = ($start);
    my $front = 0;
    
    # BFS loop
    while ($front < @queue) {
        my $current = $queue[$front++];
        
        # Visit all neighbors
        if (exists $graph{$current}) {
            for my $neighbor (@{$graph{$current}}) {
                if ($dist[$neighbor] == -1) {  # If not visited
                    $dist[$neighbor] = $dist[$current] + 1;
                    push @queue, $neighbor;
                }
            }
        }
    }
    
    return @dist;
}

# Run BFS from node 1
my @distances = bfs(1, $n);

# Output results (excluding index 0)
for my $i (1..$n) {
    print "$distances[$i] ";
}
print "\n";
```

## How it works:

1. **Input parsing**: Read the number of nodes (n) and edges (m), then build an adjacency list representation of the graph.

2. **BFS algorithm**: 
   - Initialize a distance array with -1 (unvisited) values
   - Set distance of starting node (node 1) to 0
   - Use a queue to process nodes level by level
   - For each node, visit all its neighbors and update their distances
   - Continue until queue is empty

3. **Output**: Print distances from node 1 to all other nodes, or -1 if unreachable.

## Example Input:
```
6 7
1 2
2 3
3 4
4 5
5 6
1 3
1 4
```

## Example Output:
```
1 2 2 3 4 5 
```

This represents the shortest distances from node 1 to all other nodes in the graph.

