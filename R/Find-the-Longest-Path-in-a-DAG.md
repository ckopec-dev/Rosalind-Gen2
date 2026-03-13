# Find the Longest Path in a DAG - Rosalind Problem Solution in R

## Problem Understanding

The problem asks us to find the longest path in a directed acyclic graph (DAG) from a source node to a sink node. This is a classic dynamic programming problem on DAGs.

## Approach

1. **Topological Sort**: First, we need to topologically sort the nodes to process them in the correct order
2. **Dynamic Programming**: For each node in topological order, compute the longest path to that node
3. **Path Reconstruction**: Track the actual path to reconstruct the result

## Solution

```r
# Function to find longest path in DAG
longest_path_dag <- function(edges, source, sink) {
  # Parse edges into adjacency list
  adj_list <- list()
  all_nodes <- c()
  
  for (edge in edges) {
    from <- as.numeric(unlist(strsplit(edge, " -> "))[1])
    to <- as.numeric(unlist(strsplit(edge, " -> "))[2])
    weight <- as.numeric(unlist(strsplit(edge, ": "))[2])
    
    if (is.null(adj_list[[from]])) {
      adj_list[[from]] <- list()
    }
    adj_list[[from]][[length(adj_list[[from]]) + 1]] <- list(to = to, weight = weight)
    
    all_nodes <- c(all_nodes, from, to)
  }
  
  # Get all unique nodes and sort them
  nodes <- sort(unique(all_nodes))
  
  # Topological sort using Kahn's algorithm
  in_degree <- rep(0, length(nodes))
  names(in_degree) <- nodes
  
  for (edge in edges) {
    from <- as.numeric(unlist(strsplit(edge, " -> "))[1])
    to <- as.numeric(unlist(strsplit(edge, " -> "))[2])
    in_degree[to] <- in_degree[to] + 1
  }
  
  # Initialize queue with nodes having in-degree 0
  queue <- nodes[in_degree == 0]
  topo_order <- c()
  
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    topo_order <- c(topo_order, current)
    
    if (!is.null(adj_list[[current]])) {
      for (neighbor in adj_list[[current]]) {
        to_node <- neighbor$to
        in_degree[to_node] <- in_degree[to_node] - 1
        if (in_degree[to_node] == 0) {
          queue <- c(queue, to_node)
        }
      }
    }
  }
  
  # Dynamic programming to find longest path
  # dist[i] = longest path to node i
  dist <- rep(-Inf, length(nodes))
  names(dist) <- nodes
  dist[source] <- 0
  
  # Previous node in the longest path (for path reconstruction)
  prev <- rep(0, length(nodes))
  names(prev) <- nodes
  
  # Process nodes in topological order
  for (node in topo_order) {
    if (dist[node] != -Inf) {
      if (!is.null(adj_list[[node]])) {
        for (edge_info in adj_list[[node]]) {
          to_node <- edge_info$to
          weight <- edge_info$weight
          
          if (dist[node] + weight > dist[to_node]) {
            dist[to_node] <- dist[node] + weight
            prev[to_node] <- node
          }
        }
      }
    }
  }
  
  # Reconstruct path
  path <- c()
  current <- sink
  
  while (current != 0) {
    path <- c(current, path)
    if (prev[current] == 0) break
    current <- prev[current]
  }
  
  return(list(
    distance = dist[sink],
    path = path
  ))
}

# Alternative cleaner implementation
find_longest_path <- function(edges, source, sink) {
  # Parse edges
  parsed_edges <- list()
  nodes <- c()
  
  for (edge in edges) {
    parts <- unlist(strsplit(edge, " -> "))
    from <- as.numeric(parts[1])
    to_weight <- unlist(strsplit(parts[2], ": "))
    to <- as.numeric(to_weight[1])
    weight <- as.numeric(to_weight[2])
    
    if (is.null(parsed_edges[[from]])) {
      parsed_edges[[from]] <- list()
    }
    parsed_edges[[from]][[length(parsed_edges[[from]]) + 1]] <- list(to = to, weight = weight)
    
    nodes <- c(nodes, from, to)
  }
  
  # Get all nodes and sort
  all_nodes <- sort(unique(nodes))
  
  # Topological sort using DFS
  visited <- rep(FALSE, length(all_nodes))
  topo_order <- c()
  names(visited) <- all_nodes
  
  dfs_topo <- function(node) {
    visited[node] <- TRUE
    if (!is.null(parsed_edges[[node]])) {
      for (edge in parsed_edges[[node]]) {
        if (!visited[edge$to]) {
          dfs_topo(edge$to)
        }
      }
    }
    topo_order <<- c(node, topo_order)
  }
  
  # Perform topological sort
  for (node in all_nodes) {
    if (!visited[node]) {
      dfs_topo(node)
    }
  }
  
  # DP to find longest path
  dist <- rep(-Inf, length(all_nodes))
  names(dist) <- all_nodes
  dist[source] <- 0
  
  prev <- rep(0, length(all_nodes))
  names(prev) <- all_nodes
  
  for (node in topo_order) {
    if (dist[node] != -Inf && !is.null(parsed_edges[[node]])) {
      for (edge in parsed_edges[[node]]) {
        to_node <- edge$to
        weight <- edge$weight
        
        if (dist[node] + weight > dist[to_node]) {
          dist[to_node] <- dist[node] + weight
          prev[to_node] <- node
        }
      }
    }
  }
  
  # Reconstruct path
  path <- c()
  current <- sink
  
  while (current != 0) {
    path <- c(current, path)
    if (prev[current] == 0) break
    current <- prev[current]
  }
  
  return(list(
    distance = dist[sink],
    path = path
  ))
}

# Example usage with sample data
# Sample input format:
# edges <- c(
#   "0 -> 1: 5",
#   "0 -> 2: 3",
#   "1 -> 3: 4",
#   "2 -> 3: 2",
#   "2 -> 4: 6",
#   "3 -> 5: 1",
#   "4 -> 5: 3"
# )
# 
# result <- find_longest_path(edges, 0, 5)
# print(paste("Maximum distance:", result$distance))
# print(paste("Path:", paste(result$path, collapse = " -> ")))
```

## Key Points

1. **Topological Sort**: Essential for processing nodes in the correct order
2. **Dynamic Programming**: For each node, we maintain the maximum distance to reach it
3. **Path Reconstruction**: Track previous nodes to rebuild the actual path
4. **Edge Cases**: Handle nodes with no incoming edges (source nodes)

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing the graph and DP arrays

## Usage Example

```r
# Example usage:
edges <- c(
  "0 -> 1: 5",
  "0 -> 2: 3",
  "1 -> 3: 4", 
  "2 -> 3: 2",
  "2 -> 4: 6",
  "3 -> 5: 1",
  "4 -> 5: 3"
)

result <- find_longest_path(edges, 0, 5)
cat("Maximum distance:", result$distance, "\n")
cat("Path:", paste(result$path, collapse = " -> "), "\n")
```

This solution correctly handles the longest path problem in DAGs using dynamic programming with topological ordering.

