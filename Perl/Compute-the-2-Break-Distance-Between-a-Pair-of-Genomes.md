# Rosalind Problem: Compute the 2-Break Distance Between a Pair of Genomes

## Problem Understanding

The 2-break distance between two genomes is the minimum number of 2-break operations needed to transform one genome into another. A 2-break operation cuts two edges and rejoins them in a different way.

## Solution Approach

The key insight is to use the concept of the breakpoint graph. The 2-break distance equals the number of cycles in the breakpoint graph minus the number of genomes.

## Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $input = do { local $/; <STDIN> };
my @lines = split /\n/, $input;

# Parse the genomes
my $genome1 = $lines[0];
my $genome2 = $lines[1];

# Remove parentheses and split into elements
$genome1 =~ s/[()]//g;
$genome2 =~ s/[()]//g;

my @genome1_elements = split /\s+/, $genome1;
my @genome2_elements = split /\s+/, $genome2;

# Convert to signed integers
my @g1 = map { $_ =~ /^-/ ? -$_ : $_ } @genome1_elements;
my @g2 = map { $_ =~ /^-/ ? -$_ : $_ } @genome2_elements;

# Create adjacency lists for both genomes
my %adj1 = create_adjacency_list(\@g1);
my %adj2 = create_adjacency_list(\@g2);

# Create breakpoint graph
my %breakpoint_graph = create_breakpoint_graph(\%adj1, \%adj2);

# Count cycles in the breakpoint graph
my $cycles = count_cycles(\%breakpoint_graph);

# Calculate 2-break distance
# Distance = (number of elements in genome) - (number of cycles)
my $n = @g1;
my $distance = $n - $cycles;

print "$distance\n";

# Function to create adjacency list from genome
sub create_adjacency_list {
    my ($genome) = @_;
    my %adjacency;
    
    my $len = @$genome;
    
    # Handle circular chromosomes
    for my $i (0..$len-1) {
        my $current = $genome->[$i];
        my $next = $genome->[($i + 1) % $len];
        
        # Add edges (both directions)
        $adjacency{abs($current)}{0} = $next;  # Forward edge
        $adjacency{abs($next)}{1} = $current;  # Backward edge
    }
    
    return %adjacency;
}

# Function to create breakpoint graph
sub create_breakpoint_graph {
    my ($adj1, $adj2) = @_;
    my %graph;
    
    # Add edges from first genome
    for my $node (keys %$adj1) {
        my $edge1 = $adj1->{$node}{0};
        my $edge2 = $adj1->{$node}{1};
        
        # Add edges to graph
        push @{$graph{$node}{0}}, $edge1;
        push @{$graph{$edge1}{1}}, $node;
        
        push @{$graph{$node}{1}}, $edge2;
        push @{$graph{$edge2}{0}}, $node;
    }
    
    # Add edges from second genome
    for my $node (keys %$adj2) {
        my $edge1 = $adj2->{$node}{0};
        my $edge2 = $adj2->{$node}{1};
        
        # Add edges to graph
        push @{$graph{$node}{0}}, $edge1;
        push @{$graph{$edge1}{1}}, $node;
        
        push @{$graph{$node}{1}}, $edge2;
        push @{$graph{$edge2}{0}}, $node;
    }
    
    return %graph;
}

# Function to count cycles in the breakpoint graph
sub count_cycles {
    my ($graph) = @_;
    my %visited;
    my $cycle_count = 0;
    
    for my $node (keys %$graph) {
        next if $visited{$node};
        
        # Start a new cycle
        my $current = $node;
        my $direction = 0;  # 0 for forward, 1 for backward
        my $start = $node;
        
        while (!$visited{$current} || $current != $start) {
            $visited{$current} = 1;
            
            # Find next node
            my @edges = @{$graph->{$current}{$direction}};
            my $next_node = $edges[0];
            
            # Move to next node
            $current = $next_node;
            $direction = 1 - $direction;  # Switch direction
        }
        
        $cycle_count++;
    }
    
    return $cycle_count;
}
```

## Alternative Cleaner Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $input = do { local $/; <STDIN> };
my @lines = split /\n/, $input;

# Parse genomes
my @genome1 = parse_genome($lines[0]);
my @genome2 = parse_genome($lines[1]);

# Create adjacency lists
my %adj1 = create_adjacency_list(\@genome1);
my %adj2 = create_adjacency_list(\@genome2);

# Count cycles in breakpoint graph
my $cycles = count_cycles(\%adj1, \%adj2);

# Calculate distance
my $n = @genome1;
my $distance = $n - $cycles;

print "$distance\n";

sub parse_genome {
    my ($genome_str) = @_;
    $genome_str =~ s/[()]//g;
    my @elements = split /\s+/, $genome_str;
    return map { $_ =~ /^-/ ? -$_ : $_ } @elements;
}

sub create_adjacency_list {
    my ($genome) = @_;
    my %adjacency;
    
    my $len = @$genome;
    for my $i (0..$len-1) {
        my $current = $genome->[$i];
        my $next = $genome->[($i + 1) % $len];
        
        # Forward edge
        $adjacency{abs($current)}{0} = $next;
        # Backward edge  
        $adjacency{abs($next)}{1} = $current;
    }
    
    return %adjacency;
}

sub count_cycles {
    my ($adj1, $adj2) = @_;
    my %visited;
    my $cycles = 0;
    
    # Create the breakpoint graph edges
    my %edges;
    for my $node (keys %$adj1) {
        my $edge1 = $adj1->{$node}{0};
        my $edge2 = $adj1->{$node}{1};
        push @{$edges{$node}{0}}, $edge1;
        push @{$edges{$edge1}{1}}, $node;
        push @{$edges{$node}{1}}, $edge2;
        push @{$edges{$edge2}{0}}, $node;
    }
    
    for my $node (keys %$adj2) {
        my $edge1 = $adj2->{$node}{0};
        my $edge2 = $adj2->{$node}{1};
        push @{$edges{$node}{0}}, $edge1;
        push @{$edges{$edge1}{1}}, $node;
        push @{$edges{$node}{1}}, $edge2;
        push @{$edges{$edge2}{0}}, $node;
    }
    
    # Count cycles
    for my $node (keys %edges) {
        next if $visited{$node};
        my $current = $node;
        my $start = $node;
        my $direction = 0;
        
        while (!$visited{$current} || $current != $start) {
            $visited{$current} = 1;
            my @next_edges = @{$edges{$current}{$direction}};
            my $next_node = $next_edges[0];
            $current = $next_node;
            $direction = 1 - $direction;
        }
        $cycles++;
    }
    
    return $cycles;
}
```

## Usage

To run this solution:

```bash
perl compute_2break_distance.pl < input.txt
```

Where `input.txt` contains two genomes in the format:
```
(+1 +2 +3 +4 +5 +6)
(+1 -3 -6 -5 +4 +2)
```

## Key Concepts

1. **2-break distance**: The minimum number of 2-break operations to transform one genome into another
2. **Breakpoint graph**: A graph that represents both genomes simultaneously
3. **Cycles**: The number of cycles in the breakpoint graph determines the distance
4. **Formula**: Distance = n - cycles, where n is the number of elements in the genome

The algorithm works by constructing a breakpoint graph from both genomes and counting the number of cycles, then applying the mathematical relationship to compute the 2-break distance.

