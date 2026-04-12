# Rosalind Problem: Implement 2-BreakOnGenomeGraph

## Problem Description
Implement the 2-BreakOnGenomeGraph algorithm that performs a 2-break operation on a genome graph.

## Solution in R

```r
# Function to implement 2-BreakOnGenomeGraph
2BreakOnGenomeGraph <- function(graph, i, j, k, l) {
  # Convert graph to list format if needed
  if (is.matrix(graph)) {
    graph <- list()
    for (m in 1:nrow(graph)) {
      graph[[m]] <- as.numeric(graph[m, ])
    }
  }
  
  # Find the edges containing the break points
  edges_to_remove <- c()
  edges_to_add <- c()
  
  # Find edges to remove
  for (edge in graph) {
    if ((edge[1] == i && edge[2] == j) || (edge[1] == j && edge[2] == i) ||
        (edge[1] == k && edge[2] == l) || (edge[1] == l && edge[2] == k)) {
      edges_to_remove <- c(edges_to_remove, which(sapply(graph, function(x) identical(x, edge))))
    }
  }
  
  # Remove the edges
  graph <- graph[-edges_to_remove]
  
  # Add new edges
  graph <- c(graph, list(c(i, k)), list(c(j, l)))
  
  return(graph)
}

# Alternative implementation that works with adjacency list representation
2BreakOnGenomeGraph_v2 <- function(adjacency_list, i, j, k, l) {
  # Convert to edge list format
  edges <- c()
  for (node in names(adjacency_list)) {
    for (neighbor in adjacency_list[[node]]) {
      if (as.numeric(node) < neighbor) {  # Avoid duplicates
        edges <- rbind(edges, c(as.numeric(node), neighbor))
      }
    }
  }
  
  # Find and remove edges (i,j) and (k,l)
  edges_to_remove <- c()
  for (idx in 1:nrow(edges)) {
    if ((edges[idx,1] == i && edges[idx,2] == j) || 
        (edges[idx,1] == j && edges[idx,2] == i) ||
        (edges[idx,1] == k && edges[idx,2] == l) || 
        (edges[idx,1] == l && edges[idx,2] == k)) {
      edges_to_remove <- c(edges_to_remove, idx)
    }
  }
  
  # Remove edges
  edges <- edges[-edges_to_remove, , drop = FALSE]
  
  # Add new edges (i,k) and (j,l)
  edges <- rbind(edges, c(i, k), c(j, l))
  
  # Convert back to adjacency list format
  adj_list <- list()
  for (idx in 1:nrow(edges)) {
    u <- edges[idx, 1]
    v <- edges[idx, 2]
    
    if (is.null(adj_list[[as.character(u)]])) {
      adj_list[[as.character(u)]] <- c(v)
    } else {
      adj_list[[as.character(u)]] <- c(adj_list[[as.character(u)]], v)
    }
    
    if (is.null(adj_list[[as.character(v)]])) {
      adj_list[[as.character(v)]] <- c(u)
    } else {
      adj_list[[as.character(v)]] <- c(adj_list[[as.character(v)]], u)
    }
  }
  
  return(adj_list)
}

# Simple edge list version
2BreakOnGenomeGraph_simple <- function(edges, i, j, k, l) {
  # Remove edges (i,j) and (k,l)
  new_edges <- edges
  
  # Find edges to remove
  remove_indices <- c()
  for (idx in 1:nrow(edges)) {
    if ((edges[idx,1] == i && edges[idx,2] == j) || 
        (edges[idx,1] == j && edges[idx,2] == i) ||
        (edges[idx,1] == k && edges[idx,2] == l) || 
        (edges[idx,1] == l && edges[idx,2] == k)) {
      remove_indices <- c(remove_indices, idx)
    }
  }
  
  # Remove the edges
  if (length(remove_indices) > 0) {
    new_edges <- new_edges[-remove_indices, , drop = FALSE]
  }
  
  # Add new edges
  new_edges <- rbind(new_edges, c(i, k), c(j, l))
  
  return(new_edges)
}

# Example usage:
# Example graph as edge list
example_edges <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), ncol = 2, byrow = TRUE)
print("Original edges:")
print(example_edges)

# Perform 2-break with i=1, j=6, k=3, l=8
result <- 2BreakOnGenomeGraph_simple(example_edges, 1, 6, 3, 8)
print("After 2-break:")
print(result)
```

## Explanation

This implementation provides three versions of the 2-break algorithm:

1. **Basic version**: Takes a genome graph and performs 2-break on specified edges
2. **Adjacency list version**: Works with adjacency list representation
3. **Simple edge list version**: Most straightforward implementation working with edge lists

The 2-break operation works by:
1. Removing two existing edges from the genome graph
2. Adding two new edges that connect the vertices in a different way
3. Specifically, if we have edges (i,j) and (k,l), we remove them and add edges (i,k) and (j,l)

## Key Points:
- The function takes a genome graph (as edge list) and four integers (i, j, k, l) representing the edge endpoints
- It removes the edges containing these endpoints and adds new edges
- The result is a modified genome graph after the 2-break operation
- This operation is fundamental in genome rearrangement problems

## Time Complexity:
O(n) where n is the number of edges in the genome graph

## Space Complexity:
O(n) for storing the modified graph

