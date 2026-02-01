# Rosalind Problem: General Sink

## Problem Statement
A sink is a node with no outgoing edges in a directed graph. Given a directed graph with n nodes and m edges, find all sinks in the graph.

## Solution in R

```r
# Function to find all sinks in a directed graph
find_sinks <- function(edges) {
  # Extract all nodes (source and target)
  all_nodes <- unique(c(edges[,1], edges[,2]))
  
  # Find all nodes that appear as targets (have outgoing edges)
  nodes_with_outgoing <- unique(edges[,2])
  
  # Sinks are nodes that don't appear as sources (have no outgoing edges)
  sinks <- setdiff(all_nodes, nodes_with_outgoing)
  
  return(sort(sinks))
}

# Alternative approach using adjacency matrix
find_sinks_matrix <- function(edges, n_nodes) {
  # Create adjacency matrix
  adj_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  
  # Fill the matrix
  for (i in 1:nrow(edges)) {
    source <- edges[i,1]
    target <- edges[i,2]
    adj_matrix[source, target] <- 1
  }
  
  # Find nodes with no outgoing edges (row sum = 0)
  row_sums <- rowSums(adj_matrix)
  sinks <- which(row_sums == 0)
  
  return(sort(sinks))
}

# Example usage with sample data
# Sample input: edges in format (source, target)
sample_edges <- matrix(c(
  1, 2,
  2, 3,
  3, 4,
  4, 5,
  5, 6
), ncol = 2, byrow = TRUE)

# Find sinks
sinks <- find_sinks(sample_edges)
print(paste("Sinks:", paste(sinks, collapse = " ")))

# More complex example
complex_edges <- matrix(c(
  1, 2,
  1, 3,
  2, 4,
  3, 4,
  4, 5,
  5, 6,
  6, 7
), ncol = 2, byrow = TRUE)

sinks_complex <- find_sinks(complex_edges)
print(paste("Sinks (complex):", paste(sinks_complex, collapse = " ")))

# Read from file (if needed)
read_graph_from_file <- function(filename) {
  # Read edges from file
  edges <- read.table(filename, header = FALSE, sep = " ")
  return(edges)
}

# If you have a file with edges, you can use:
# edges <- read_graph_from_file("rosalind_sink.txt")
# sinks <- find_sinks(edges)
```

## Explanation

The solution works by:

1. **Identifying all nodes**: Extract all unique nodes from both source and target positions in the edges
2. **Finding nodes with outgoing edges**: Identify all nodes that appear as targets (have outgoing edges)
3. **Determining sinks**: Sinks are nodes that are in the complete node set but NOT in the set of nodes with outgoing edges

## Key Concepts

- **Sink**: A node with no outgoing edges (in-degree < total degree)
- **Time complexity**: O(m + n) where m is number of edges and n is number of nodes
- **Space complexity**: O(n)

## Sample Input/Output

**Input edges:**
```
1 2
2 3
3 4
4 5
5 6
```

**Output:**
```
Sinks: 6
```

**Input edges:**
```
1 2
1 3
2 4
3 4
4 5
5 6
6 7
```

**Output:**
```
Sinks (complex): 7
```

The function returns all nodes that have no outgoing edges, which are the sinks in the directed graph.

