# Rosalind Problem: Strongly Connected Components

## Problem Description

In the previous problem, we learned how to compute the strongly connected components of a directed graph. In this problem, we are given a directed graph with n nodes and m edges, and we need to compute the number of strongly connected components in the graph.

## Solution Approach

To solve this problem, we'll use Kosaraju's algorithm:
1. Perform a DFS on the original graph to get the finishing times
2. Reverse the graph edges
3. Perform DFS on the reversed graph in the order of decreasing finishing times
4. Each DFS tree corresponds to a strongly connected component

## R Implementation

```r
# Function to solve Strongly Connected Components problem
scc <- function(n, edges) {
  # Build adjacency list representation
  adj_list <- list()
  for (i in 1:n) {
    adj_list[[as.character(i)]] <- c()
  }
  
  # Populate adjacency list
  for (i in 1:length(edges)) {
    from <- as.character(edges[i][1])
    to <- as.character(edges[i][2])
    adj_list[[from]] <- c(adj_list[[from]], to)
  }
  
  # Step 1: Get finishing times using DFS
  visited <- rep(FALSE, n)
  finish_stack <- c()
  
  dfs_finish <- function(node) {
    visited[as.numeric(node)] <- TRUE
    neighbors <- adj_list[[node]]
    for (neighbor in neighbors) {
      if (!visited[as.numeric(neighbor)]) {
        dfs_finish(neighbor)
      }
    }
    finish_stack <<- c(finish_stack, node)
  }
  
  # Run DFS for all unvisited nodes
  for (i in 1:n) {
    if (!visited[i]) {
      dfs_finish(as.character(i))
    }
  }
  
  # Step 2: Create reversed graph
  rev_adj_list <- list()
  for (i in 1:n) {
    rev_adj_list[[as.character(i)]] <- c()
  }
  
  for (i in 1:length(edges)) {
    from <- as.character(edges[i][1])
    to <- as.character(edges[i][2])
    rev_adj_list[[to]] <- c(rev_adj_list[[to]], from)
  }
  
  # Step 3: DFS on reversed graph in reverse finishing time order
  visited <- rep(FALSE, n)
  scc_count <- 0
  
  dfs_scc <- function(node) {
    visited[as.numeric(node)] <- TRUE
    neighbors <- rev_adj_list[[node]]
    for (neighbor in neighbors) {
      if (!visited[as.numeric(neighbor)]) {
        dfs_scc(neighbor)
      }
    }
  }
  
  # Process nodes in reverse order of finishing times
  while (length(finish_stack) > 0) {
    node <- finish_stack[length(finish_stack)]
    finish_stack <- finish_stack[-length(finish_stack)]
    
    if (!visited[as.numeric(node)]) {
      dfs_scc(node)
      scc_count <- scc_count + 1
    }
  }
  
  return(scc_count)
}

# Example usage
# For the sample input: n=5, edges = cbind(c(1,2,3,4,5), c(2,3,1,5,4))
# This represents edges: 1->2, 2->3, 3->1, 4->5, 5->4

# Sample test case
n <- 5
edges <- rbind(c(1,2), c(2,3), c(3,1), c(4,5), c(5,4))

result <- scc(n, edges)
print(paste("Number of strongly connected components:", result))
```

## Alternative Implementation (More Efficient)

```r
# More efficient implementation using adjacency matrix
strongly_connected_components <- function(n, edges) {
  # Create adjacency matrix
  adj <- matrix(0, nrow=n, ncol=n)
  
  for (i in 1:nrow(edges)) {
    from <- edges[i,1]
    to <- edges[i,2]
    adj[from, to] <- 1
  }
  
  # Kosaraju's algorithm
  # Step 1: Get finishing times
  visited <- rep(FALSE, n)
  finish_stack <- c()
  
  dfs_finish <- function(node) {
    visited[node] <- TRUE
    for (j in 1:n) {
      if (adj[node, j] == 1 && !visited[j]) {
        dfs_finish(j)
      }
    }
    finish_stack <<- c(finish_stack, node)
  }
  
  for (i in 1:n) {
    if (!visited[i]) {
      dfs_finish(i)
    }
  }
  
  # Step 2: Transpose the adjacency matrix
  adj_t <- t(adj)
  
  # Step 3: DFS on transposed graph
  visited <- rep(FALSE, n)
  scc_count <- 0
  
  dfs_scc <- function(node) {
    visited[node] <- TRUE
    for (j in 1:n) {
      if (adj_t[node, j] == 1 && !visited[j]) {
        dfs_scc(j)
      }
    }
  }
  
  # Process in reverse finishing time order
  while (length(finish_stack) > 0) {
    node <- finish_stack[length(finish_stack)]
    finish_stack <- finish_stack[-length(finish_stack)]
    
    if (!visited[node]) {
      dfs_scc(node)
      scc_count <- scc_count + 1
    }
  }
  
  return(scc_count)
}

# Example usage with sample data
edges <- rbind(
  c(1, 2),
  c(2, 3),
  c(3, 1),
  c(4, 5),
  c(5, 4)
)

result <- strongly_connected_components(5, edges)
print(result)
```

## Key Points

1. **Kosaraju's Algorithm**: The algorithm uses two DFS traversals to find strongly connected components
2. **Time Complexity**: O(V + E) where V is vertices and E is edges
3. **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures
4. **Important**: The algorithm works by first finding finishing times, then reversing the graph and processing nodes in reverse order of finishing times

## Sample Input/Output

Input:
```
n = 5
edges = [[1,2], [2,3], [3,1], [4,5], [5,4]]
```

Output:
```
2
```

This indicates there are 2 strongly connected components in the graph: {1,2,3} and {4,5}.

