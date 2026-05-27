# Rosalind Problem: Shortest Cycle Through a Given Edge

## Problem Understanding

We need to find the shortest cycle in an undirected graph that passes through a specific given edge.

## Approach

1. **Graph Representation**: Use adjacency list representation
2. **Algorithm**: 
   - Remove the given edge from the graph
   - Find the shortest path between the two vertices of the removed edge
   - Add the edge back and return the path length + 1 (for the removed edge)
3. **Implementation**: Use BFS to find shortest path

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

# Read input
my @lines = <STDIN>;
chomp @lines;

# Parse the graph
my @graph_data = split /\s+/, $lines[0];
my $n = $graph_data[0];  # number of vertices
my $m = $graph_data[1];  # number of edges

# Parse the edge to include in cycle
my @edge_data = split /\s+/, $lines[1];
my $u = $edge_data[0] - 1;  # convert to 0-indexed
my $v = $edge_data[1] - 1;

# Build adjacency list
my @adj = ();
for my $i (0..$n-1) {
    $adj[$i] = [];
}

# Read edges and build adjacency list
for my $i (2..$m+1) {
    my @edge = split /\s+/, $lines[$i];
    my $x = $edge[0] - 1;  # convert to 0-indexed
    my $y = $edge[1] - 1;
    
    push @{$adj[$x]}, $y;
    push @{$adj[$y]}, $x;
}

# BFS to find shortest path between two vertices
sub bfs_shortest_path {
    my ($start, $end, $adj_ref, $n) = @_;
    
    my @visited = (0) x $n;
    my @distance = (0) x $n;
    my @queue = ($start);
    $visited[$start] = 1;
    
    while (@queue) {
        my $current = shift @queue;
        
        if ($current == $end) {
            return $distance[$end];
        }
        
        for my $neighbor (@{$adj_ref->[$current]}) {
            if (!$visited[$neighbor]) {
                $visited[$neighbor] = 1;
                $distance[$neighbor] = $distance[$current] + 1;
                push @queue, $neighbor;
            }
        }
    }
    
    return -1;  # No path found
}

# Remove the given edge temporarily
my @temp_adj = map { [@{$adj[$_]}] } 0..$n-1;

# Remove edge (u,v) from adjacency list
my $found_u = 0;
my $found_v = 0;

for my $i (0..$#{$temp_adj[$u]}) {
    if ($temp_adj[$u][$i] == $v) {
        splice @{$temp_adj[$u]}, $i, 1;
        $found_u = 1;
        last;
    }
}

for my $i (0..$#{$temp_adj[$v]}) {
    if ($temp_adj[$v][$i] == $u) {
        splice @{$temp_adj[$v]}, $i, 1;
        $found_v = 1;
        last;
    }
}

# Find shortest path between u and v in the modified graph
my $shortest_path_length = bfs_shortest_path($u, $v, \@temp_adj, $n);

# The shortest cycle length is the shortest path + 1 (the removed edge)
if ($shortest_path_length == -1) {
    print "No cycle exists through the given edge\n";
} else {
    print $shortest_path_length + 1 . "\n";
}
```

## Explanation

1. **Input Parsing**: 
   - First line contains number of vertices and edges
   - Second line contains the edge vertices (1-indexed)
   - Remaining lines contain the edges

2. **Graph Construction**: 
   - Build adjacency list representation
   - Convert to 0-indexed vertices

3. **Algorithm**:
   - Temporarily remove the given edge from the graph
   - Use BFS to find the shortest path between the two vertices of the removed edge
   - The shortest cycle length = shortest path length + 1 (for the removed edge)

4. **Edge Cases**:
   - If no path exists between the vertices, there's no cycle through that edge
   - BFS ensures we find the shortest path

## Time Complexity
- O(V + E) for BFS traversal
- O(E) for building adjacency list
- Overall: O(V + E)

## Space Complexity
- O(V + E) for storing the adjacency list and BFS structures

This solution correctly handles the shortest cycle through a given edge by leveraging BFS to find the shortest path in the graph with the edge temporarily removed.

