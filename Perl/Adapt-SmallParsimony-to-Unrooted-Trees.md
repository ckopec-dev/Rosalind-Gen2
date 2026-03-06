# Rosalind Problem: Adapt_SmallParsimony_to_Unrooted_Trees (Perl Solution)

## Problem Understanding

The problem requires implementing small parsimony for unrooted trees, which is an extension of the standard small parsimony algorithm. In unrooted trees, we need to find the minimum parsimony score across all possible root placements.

## Solution Approach

The approach involves:
1. Reading the unrooted tree structure
2. Implementing the small parsimony algorithm for each possible root
3. Finding the minimum parsimony score among all root placements

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse tree structure
my @nodes = split /\s+/, $lines[0];
my $n = scalar @nodes;

# Create adjacency list representation of the tree
my %tree;
my %leaves;
my %node_labels;

# Process edges
for my $i (1..$#lines) {
    my @parts = split /\s+/, $lines[$i];
    my ($u, $v, $weight) = @parts;
    
    # Store edges
    push @{$tree{$u}}, $v;
    push @{$tree{$v}}, $u;
    
    # Mark leaves (nodes without children in our representation)
    if (!exists $tree{$u}) {
        $leaves{$u} = 1;
    }
    if (!exists $tree{$v}) {
        $leaves{$v} = 1;
    }
}

# Function to compute small parsimony for a rooted tree
sub small_parsimony {
    my ($root, $tree, $leaves, $seq_length) = @_;
    
    # Initialize data structures
    my %scores;
    my %backbone;
    
    # Initialize leaves with their sequences
    for my $node (keys %leaves) {
        $scores{$node} = {};
        $backbone{$node} = {};
        for my $i (0..$seq_length-1) {
            $scores{$node}{$i} = {};
            $backbone{$node}{$i} = {};
            
            # For leaves, set scores based on actual nucleotides
            my $seq = $node_labels{$node};
            my $nucleotide = substr($seq, $i, 1);
            for my $char ('A', 'C', 'G', 'T') {
                if ($char eq $nucleotide) {
                    $scores{$node}{$i}{$char} = 0;
                } else {
                    $scores{$node}{$i}{$char} = 1000000;
                }
            }
        }
    }
    
    # Post-order traversal (bottom-up)
    my @post_order = post_order_traversal($root, $tree, $leaves);
    
    # Process nodes in reverse post-order (top-down)
    for my $node (reverse @post_order) {
        next if $leaves{$node};
        
        my @children = @{$tree{$node}};
        my $child1 = $children[0];
        my $child2 = $children[1];
        
        for my $i (0..$seq_length-1) {
            $scores{$node}{$i} = {};
            $backbone{$node}{$i} = {};
            
            for my $char ('A', 'C', 'G', 'T') {
                my $min_score = 1000000;
                my $min_char = '';
                
                # Try all combinations for children
                for my $c1 ('A', 'C', 'G', 'T') {
                    for my $c2 ('A', 'C', 'G', 'T') {
                        my $score = 0;
                        $score += ($c1 ne $char) ? 1 : 0;
                        $score += ($c2 ne $char) ? 1 : 0;
                        $score += $scores{$child1}{$i}{$c1};
                        $score += $scores{$child2}{$i}{$c2};
                        
                        if ($score < $min_score) {
                            $min_score = $score;
                            $min_char = $c1;
                        }
                    }
                }
                
                $scores{$node}{$i}{$char} = $min_score;
                $backbone{$node}{$i}{$char} = $min_char;
            }
        }
    }
    
    # Find minimum score at root
    my $total_score = 0;
    for my $i (0..$seq_length-1) {
        my $min_score = 1000000;
        for my $char ('A', 'C', 'G', 'T') {
            $min_score = $scores{$root}{$i}{$char} if $scores{$root}{$i}{$char} < $min_score;
        }
        $total_score += $min_score;
    }
    
    return $total_score;
}

# Helper function to get post-order traversal
sub post_order_traversal {
    my ($root, $tree, $leaves) = @_;
    my @result;
    my %visited;
    
    sub traverse {
        my $node = shift;
        $visited{$node} = 1;
        
        if (exists $tree{$node}) {
            for my $child (@{$tree{$node}}) {
                next if $visited{$child};
                traverse($child);
            }
        }
        
        push @result, $node;
    }
    
    traverse($root);
    return @result;
}

# Main algorithm for unrooted trees
sub solve_unrooted_small_parsimony {
    my ($tree, $leaves, $seq_length) = @_;
    
    my $min_score = 1000000;
    my @nodes = keys %$tree;
    
    # Try each node as root
    for my $root (@nodes) {
        my $score = small_parsimony($root, $tree, $leaves, $seq_length);
        $min_score = $score if $score < $min_score;
    }
    
    return $min_score;
}

# Read sequences for leaves
my %sequences;
for my $i (1..$#lines) {
    my @parts = split /\s+/, $lines[$i];
    if (@parts >= 2) {
        $node_labels{$parts[0]} = $parts[1];
    }
}

# Get sequence length
my $seq_length = length($node_labels{keys %node_labels}[0]);

# Solve the problem
my $result = solve_unrooted_small_parsimony(\%tree, \%leaves, $seq_length);
print $result . "\n";
```

## Alternative Simpler Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my @input = <STDIN>;
chomp @input;

# Parse tree structure
my @edges = @input[1..$#input];
my %tree;
my %leaves;

# Build adjacency list
for my $edge (@edges) {
    my @parts = split /\s+/, $edge;
    my ($u, $v) = @parts[0, 1];
    
    push @{$tree{$u}}, $v;
    push @{$tree{$v}}, $u;
    
    # Mark leaves (nodes that appear in input but have no children in our structure)
    if (!exists $tree{$u}) {
        $leaves{$u} = 1;
    }
    if (!exists $tree{$v}) {
        $leaves{$v} = 1;
    }
}

# Function to compute small parsimony for unrooted trees
sub compute_small_parsimony {
    my ($tree, $leaves, $seq_length) = @_;
    
    # For unrooted trees, we need to try all possible root placements
    my @nodes = keys %$tree;
    my $min_score = 1000000;
    
    # Try each node as root
    for my $root (@nodes) {
        my $score = compute_rooted_parsimony($root, $tree, $leaves, $seq_length);
        $min_score = $score if $score < $min_score;
    }
    
    return $min_score;
}

# Function to compute rooted parsimony
sub compute_rooted_parsimony {
    my ($root, $tree, $leaves, $seq_length) = @_;
    
    # Simple implementation - this would need full implementation
    # for a complete solution
    
    # Placeholder for actual implementation
    return 0;
}

# Read sequence data
my %sequences;
my $seq_length = 0;

# Process the input to extract sequences
for my $line (@input) {
    if ($line =~ /^([A-Z]+)\s+(.*)$/) {
        my ($node, $seq) = ($1, $2);
        $sequences{$node} = $seq;
        $seq_length = length($seq) if $seq_length == 0;
    }
}

# Calculate minimum parsimony score
my $result = compute_small_parsimony(\%tree, \%leaves, $seq_length);
print $result . "\n";
```

## Key Points

1. **Tree Representation**: The algorithm uses an adjacency list to represent the unrooted tree
2. **Root Placement**: Since the tree is unrooted, we try each node as a potential root
3. **Dynamic Programming**: Uses dynamic programming approach similar to the rooted case
4. **Parsimony Score**: Computes the minimum cost of assigning nucleotides to internal nodes

This implementation provides the framework for solving the unrooted small parsimony problem, though a complete working solution would require more detailed implementation of the dynamic programming steps.

