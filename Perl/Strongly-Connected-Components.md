# Rosalind Problem: Strongly Connected Components (SCC)

## Problem Understanding

In graph theory, a **strongly connected component** (SCC) of a directed graph is a subgraph where every vertex is reachable from every other vertex in that subgraph. The task is to find all strongly connected components in a directed graph.

## Approach

I'll use **Kosaraju's algorithm** which involves:
1. Finding the transpose graph
2. Performing DFS on the original graph to get finishing times
3. Performing DFS on the transpose graph in reverse order of finishing times

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse the graph
my %graph = ();
my %reverse_graph = ();
my %visited = ();
my @finish_order = ();
my %sccs = ();

# Parse edges
foreach my $line (@lines) {
    next if $line =~ /^\s*$/;
    my ($from, $to) = split /\s+/, $line;
    push @{$graph{$from}}, $to;
    push @{$reverse_graph{$to}}, $from;
}

# Step 1: DFS on original graph to get finishing times
sub dfs_forward {
    my ($node) = @_;
    $visited{$node} = 1;
    
    if (exists $graph{$node}) {
        foreach my $neighbor (@{$graph{$node}}) {
            if (!exists $visited{$neighbor}) {
                dfs_forward($neighbor);
            }
        }
    }
    
    push @finish_order, $node;
}

# Step 2: DFS on reverse graph in reverse finish order
sub dfs_reverse {
    my ($node, $scc_id) = @_;
    $visited{$node} = 1;
    $sccs{$node} = $scc_id;
    
    if (exists $reverse_graph{$node}) {
        foreach my $neighbor (@{$reverse_graph{$node}}) {
            if (!exists $visited{$neighbor}) {
                dfs_reverse($neighbor, $scc_id);
            }
        }
    }
}

# First DFS pass on original graph
my @nodes = keys %graph;
foreach my $node (@nodes) {
    if (!exists $visited{$node}) {
        dfs_forward($node);
    }
}

# Reset visited for second pass
%visited = ();

# Second DFS pass on reverse graph
my $scc_id = 0;
for my $i (reverse 0..$#finish_order) {
    my $node = $finish_order[$i];
    if (!exists $visited{$node}) {
        dfs_reverse($node, $scc_id);
        $scc_id++;
    }
}

# Count SCCs and output results
my %scc_count = ();
foreach my $node (keys %sccs) {
    my $scc = $sccs{$node};
    $scc_count{$scc}++;
}

# Output number of SCCs
print scalar keys %scc_count, "\n";

# Output SCCs in order of their first node
my @scc_list = sort { $a <=> $b } keys %scc_count;
foreach my $scc (@scc_list) {
    my @nodes_in_scc = grep { $sccs{$_} == $scc } keys %sccs;
    print join(" ", sort { $a <=> $b } @nodes_in_scc), "\n";
}
```

## Example Usage

For input:
```
1 2
2 3
3 1
3 4
4 5
5 6
6 4
```

The algorithm will output the strongly connected components.

## Time Complexity

- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing the graph and auxiliary data structures

## Key Points

1. **Kosaraju's Algorithm** is used for finding SCCs in directed graphs
2. The algorithm uses two DFS traversals with a transpose graph
3. The finishing times determine the order for the second DFS
4. Each strongly connected component is output as a sorted list of nodes

This solution handles the standard input format for Rosalind problems and correctly identifies all strongly connected components in the given directed graph.

