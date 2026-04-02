# Rosalind Problem: Square_in_a_Graph

## Problem Understanding

A square in a graph is a cycle of length 4 (a 4-cycle). Given a graph, we need to determine if it contains a square.

## Solution Approach

I'll use the NetworkX library in R to detect 4-cycles in the graph. The approach involves:
1. Reading the graph from input
2. Finding all cycles of length 4
3. Returning whether any such cycles exist

## R Implementation

```r
# Load required libraries
library(igraph)

# Function to find if a graph contains a square (4-cycle)
has_square <- function(graph) {
  # Get all cycles of length 4
  cycles <- cliquelist(graph, min = 4, max = 4)
  
  # For a 4-cycle, we can also use the following approach:
  # Find all cycles of length 4 directly
  cycles_4 <- list()
  
  # Get the adjacency matrix
  adj_matrix <- as_adjacency_matrix(graph, type = "both")
  
  # Find all 4-cycles by checking all possible 4-node combinations
  vertices <- V(graph)
  n <- length(vertices)
  
  # Check all combinations of 4 vertices
  for (i in 1:(n-3)) {
    for (j in (i+1):(n-2)) {
      for (k in (j+1):(n-1)) {
        for (l in (k+1):n) {
          # Check if these 4 vertices form a cycle
          nodes <- c(i, j, k, l)
          # Check if all edges exist
          edges_exist <- TRUE
          
          # Check edges: i-j, j-k, k-l, l-i
          if (!adj_matrix[i, j] || !adj_matrix[j, k] || 
              !adj_matrix[k, l] || !adj_matrix[l, i]) {
            edges_exist <- FALSE
          }
          
          if (edges_exist) {
            # Verify it's a cycle (not just a complete graph)
            # Check that we have exactly 4 edges forming a cycle
            cycle_edges <- cbind(c(i, j, k, l), c(j, k, l, i))
            # Check if this forms a valid 4-cycle
            cycles_4[[length(cycles_4) + 1]] <- nodes
          }
        }
      }
    }
  }
  
  # Simpler approach using igraph's cycle detection
  # Find all cycles of length 4
  all_cycles <- list()
  
  # Get all cycles using a more direct approach
  # For small graphs, we can enumerate all possible 4-cycles
  vertices <- as.vector(V(graph))
  n <- length(vertices)
  
  # Generate all possible 4-vertex combinations
  combinations <- combn(vertices, 4)
  square_found <- FALSE
  
  for (i in 1:ncol(combinations)) {
    nodes <- combinations[, i]
    
    # Create subgraph with these 4 nodes
    subgraph <- induced_subgraph(graph, nodes)
    
    # Check if subgraph is a cycle (4 edges, each node degree 2)
    if (ecount(subgraph) == 4) {
      # Check if all vertices have degree 2
      degrees <- degree(subgraph)
      if (all(degrees == 2)) {
        square_found <- TRUE
        break
      }
    }
  }
  
  return(square_found)
}

# Alternative cleaner approach using a more direct method
square_in_graph <- function(graph) {
  # Find all 4-cycles in the graph
  # Use the fact that a 4-cycle has exactly 4 edges and 4 vertices
  
  # Get all vertices
  vertices <- as.vector(V(graph))
  n <- length(vertices)
  
  # Check all possible 4-vertex combinations
  for (i in 1:(n-3)) {
    for (j in (i+1):(n-2)) {
      for (k in (j+1):(n-1)) {
        for (l in (l+1):n) {
          # Check if these 4 vertices form a 4-cycle
          nodes <- c(i, j, k, l)
          
          # Check if all edges exist to form a square
          # Edges: i-j, j-k, k-l, l-i
          if (are_adjacent(graph, i, j) && 
              are_adjacent(graph, j, k) && 
              are_adjacent(graph, k, l) && 
              are_adjacent(graph, l, i)) {
            return(TRUE)
          }
        }
      }
    }
  }
  
  return(FALSE)
}

# Function to check if two vertices are adjacent
are_adjacent <- function(graph, v1, v2) {
  return(ecount(subgraph.edges(graph, c(v1, v2), directed = FALSE)) > 0)
}

# Read input and solve
solve_square_in_graph <- function(input_file = NULL) {
  # If no input file, create a sample graph for testing
  if (is.null(input_file)) {
    # Sample graph: 5 vertices, edges: 1-2, 2-3, 3-4, 4-5, 5-1, 1-3, 2-4
    # This creates a square (1-2-3-4-1) plus diagonal 1-3
    edges <- c(1, 2, 2, 3, 3, 4, 4, 5, 5, 1, 1, 3, 2, 4)
    graph <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE))
  } else {
    # Read from file
    # Format: first line has n vertices, m edges
    # Next m lines have two integers representing edges
    lines <- readLines(input_file)
    first_line <- strsplit(lines[1], " ")[[1]]
    n <- as.numeric(first_line[1])
    m <- as.numeric(first_line[2])
    
    edges <- matrix(nrow = m, ncol = 2)
    for (i in 1:m) {
      edge_line <- strsplit(lines[i + 1], " ")[[1]]
      edges[i, ] <- as.numeric(edge_line)
    }
    
    graph <- graph_from_edgelist(edges)
  }
  
  # Check if graph contains a square
  result <- square_in_graph(graph)
  
  return(result)
}

# Example usage:
# For a sample input, let's create a simple case
# Create a graph with a square (1-2-3-4-1)
edges <- c(1, 2, 2, 3, 3, 4, 4, 1)
graph <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE))

# Test the function
has_square <- square_in_graph(graph)
print(has_square)

# For a graph without square
edges2 <- c(1, 2, 2, 3, 3, 4, 4, 5)
graph2 <- graph_from_edgelist(matrix(edges2, ncol = 2, byrow = TRUE))

has_square2 <- square_in_graph(graph2)
print(has_square2)
```

## More Efficient Implementation

```r
# More efficient approach using igraph's built-in functions
square_in_graph_efficient <- function(graph) {
  # Get the adjacency matrix
  adj_matrix <- as_adjacency_matrix(graph, type = "both")
  
  # Get all vertices
  vertices <- as.vector(V(graph))
  n <- length(vertices)
  
  # Check all combinations of 4 vertices for a square
  for (i in 1:(n-3)) {
    for (j in (i+1):(n-2)) {
      for (k in (j+1):(n-1)) {
        for (l in (k+1):n) {
          # Check if these 4 vertices form a square
          if (adj_matrix[i, j] && adj_matrix[j, k] && 
              adj_matrix[k, l] && adj_matrix[l, i]) {
            return(TRUE)
          }
        }
      }
    }
  }
  
  return(FALSE)
}

# Complete solution for Rosalind problem
square_in_graph_complete <- function(input_data) {
  # Parse input data
  lines <- strsplit(input_data, "\n")[[1]]
  
  # First line: number of vertices and edges
  first_line <- strsplit(lines[1], " ")[[1]]
  n_vertices <- as.numeric(first_line[1])
  n_edges <- as.numeric(first_line[2])
  
  # Read edges
  edges <- matrix(nrow = n_edges, ncol = 2)
  for (i in 1:n_edges) {
    edge_line <- strsplit(lines[i + 1], " ")[[1]]
    edges[i, ] <- as.numeric(edge_line)
  }
  
  # Create graph
  graph <- graph_from_edgelist(edges)
  
  # Check for square
  result <- square_in_graph_efficient(graph)
  
  return(ifelse(result, 1, 0))
}

# Example usage:
# input <- "4 5\n1 2\n2 3\n3 4\n4 1\n1 3"
# result <- square_in_graph_complete(input)
# print(result)
```

## Key Points:

1. **Problem**: Find if a graph contains a 4-cycle (square)
2. **Approach**: Check all combinations of 4 vertices to see if they form a cycle
3. **Time Complexity**: O(n⁴) in worst case, where n is the number of vertices
4. **Space Complexity**: O(1) additional space

The solution checks all possible 4-vertex combinations and verifies if they form a square by checking that all 4 edges exist between consecutive vertices in the cycle.

