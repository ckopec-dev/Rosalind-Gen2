# Rosalind Problem: Implement the Neighbor Joining Algorithm

I'll solve the Neighbor Joining algorithm implementation problem in R.

```r
# Function to implement Neighbor Joining Algorithm
neighbor_joining <- function(distances) {
  # Convert distances to matrix if it's a list
  if (is.list(distances)) {
    distances <- do.call(rbind, distances)
  }
  
  # Get number of taxa
  n <- nrow(distances)
  
  # Initialize tree structure
  tree <- list()
  
  # Create initial list of nodes
  nodes <- 1:n
  
  # While we have more than 2 nodes
  while (length(nodes) > 2) {
    # Calculate Q matrix
    Q <- matrix(0, nrow = length(nodes), ncol = length(nodes))
    
    # Calculate Q matrix values
    for (i in 1:length(nodes)) {
      for (j in 1:length(nodes)) {
        if (i != j) {
          # Sum of distances from node i and j to all other nodes
          sum_i <- sum(distances[nodes[i], nodes[!nodes %in% nodes[i]]])
          sum_j <- sum(distances[nodes[j], nodes[!nodes %in% nodes[j]]])
          Q[i, j] <- (n - 2) * distances[nodes[i], nodes[j]] - sum_i - sum_j
        }
      }
    }
    
    # Find minimum Q value
    min_q <- min(Q[Q != 0])
    min_indices <- which(Q == min_q, arr.ind = TRUE)
    
    # Get the pair of nodes to join
    i <- min_indices[1, 1]
    j <- min_indices[1, 2]
    
    # Calculate branch lengths
    node_i <- nodes[i]
    node_j <- nodes[j]
    
    # Calculate the distance from node i to the new internal node
    sum_i <- sum(distances[node_i, nodes[!nodes %in% node_i]])
    sum_j <- sum(distances[node_j, nodes[!nodes %in% node_j]])
    
    branch_i <- (distances[node_i, node_j] + (sum_i - sum_j) / (n - 2)) / 2
    branch_j <- (distances[node_i, node_j] + (sum_j - sum_i) / (n - 2)) / 2
    
    # Create new internal node
    new_node <- max(nodes) + 1
    
    # Add to tree
    tree[[length(tree) + 1]] <- list(
      node1 = node_i,
      node2 = node_j,
      branch1 = branch_i,
      branch2 = branch_j,
      new_node = new_node
    )
    
    # Remove joined nodes from nodes list
    nodes <- nodes[!nodes %in% c(node_i, node_j)]
    
    # Add new node
    nodes <- c(nodes, new_node)
    
    # Update distance matrix for the new node
    # Calculate distances from new node to all remaining nodes
    for (k in 1:length(nodes)) {
      if (nodes[k] != new_node) {
        distances[new_node, nodes[k]] <- (distances[node_i, nodes[k]] + 
                                         distances[node_j, nodes[k]] - 
                                         distances[node_i, node_j]) / 2
        distances[nodes[k], new_node] <- distances[new_node, nodes[k]]
      }
    }
    
    # Reduce matrix size
    n <- n - 1
  }
  
  # Add final two nodes
  tree[[length(tree) + 1]] <- list(
    node1 = nodes[1],
    node2 = nodes[2],
    branch1 = distances[nodes[1], nodes[2]] / 2,
    branch2 = distances[nodes[1], nodes[2]] / 2,
    new_node = NA
  )
  
  return(tree)
}

# Alternative simpler implementation for the specific problem
neighbor_joining_simple <- function(d) {
  # d is a distance matrix
  n <- nrow(d)
  
  # Convert to list for easier handling
  if (is.matrix(d)) {
    d <- as.list(d)
  }
  
  # Create node labels
  nodes <- 1:n
  
  # Initialize tree
  tree <- list()
  
  # While more than 2 nodes
  while (length(nodes) > 2) {
    # Calculate Q matrix
    Q <- matrix(0, nrow = length(nodes), ncol = length(nodes))
    
    for (i in 1:length(nodes)) {
      for (j in 1:length(nodes)) {
        if (i != j) {
          # Sum of distances from node i and j to all other nodes
          sum_i <- sum(d[[nodes[i]]][!names(d[[nodes[i]]]) %in% names(nodes[i])])
          sum_j <- sum(d[[nodes[j]]][!names(d[[nodes[j]]]) %in% names(nodes[j])])
          Q[i, j] <- (n - 2) * d[nodes[i], nodes[j]] - sum_i - sum_j
        }
      }
    }
    
    # Find minimum Q value
    min_q <- min(Q[Q != 0])
    min_indices <- which(Q == min_q, arr.ind = TRUE)
    
    # Get nodes to join
    i <- min_indices[1, 1]
    j <- min_indices[1, 2]
    
    # Calculate branch lengths
    node_i <- nodes[i]
    node_j <- nodes[j]
    
    # Calculate branch lengths
    sum_i <- sum(d[[node_i]][-i])
    sum_j <- sum(d[[node_j]][-j])
    
    branch_i <- (d[node_i, node_j] + (sum_i - sum_j) / (n - 2)) / 2
    branch_j <- (d[node_i, node_j] + (sum_j - sum_i) / (n - 2)) / 2
    
    # Create new node
    new_node <- max(nodes) + 1
    
    # Add to tree
    tree[[length(tree) + 1]] <- list(
      node1 = node_i,
      node2 = node_j,
      branch1 = branch_i,
      branch2 = branch_j,
      new_node = new_node
    )
    
    # Update nodes list
    nodes <- nodes[!nodes %in% c(node_i, node_j)]
    nodes <- c(nodes, new_node)
    
    # Update distance matrix
    for (k in 1:length(nodes)) {
      if (nodes[k] != new_node) {
        d[new_node, nodes[k]] <- (d[node_i, nodes[k]] + 
                                 d[node_j, nodes[k]] - 
                                 d[node_i, node_j]) / 2
        d[nodes[k], new_node] <- d[new_node, nodes[k]]
      }
    }
    
    n <- n - 1
  }
  
  # Add final connection
  tree[[length(tree) + 1]] <- list(
    node1 = nodes[1],
    node2 = nodes[2],
    branch1 = d[nodes[1], nodes[2]] / 2,
    branch2 = d[nodes[1], nodes[2]] / 2,
    new_node = NA
  )
  
  return(tree)
}

# Read and parse input function
read_distance_matrix <- function(filename) {
  # This function would read from file
  # For demonstration, we'll create a sample matrix
  # In practice, you'd read from file using read.table or similar
  return(matrix(c(
    0, 5, 9, 9,
    5, 0, 10, 10,
    9, 10, 0, 8,
    9, 10, 8, 0
  ), nrow = 4, byrow = TRUE))
}

# Main execution function
solve_neighbor_joining <- function() {
  # Read input (assuming it's a distance matrix)
  # For this example, we'll use a sample matrix
  d <- matrix(c(
    0, 5, 9, 9,
    5, 0, 10, 10,
    9, 10, 0, 8,
    9, 10, 8, 0
  ), nrow = 4, byrow = TRUE)
  
  # Apply neighbor joining
  result <- neighbor_joining_simple(d)
  
  # Print results
  cat("Neighbor Joining Tree:\n")
  for (i in 1:length(result)) {
    cat("Step", i, ":\n")
    cat("  Join nodes", result[[i]]$node1, "and", result[[i]]$node2, "\n")
    cat("  Branch lengths:", result[[i]]$branch1, "and", result[[i]]$branch2, "\n")
    if (!is.na(result[[i]]$new_node)) {
      cat("  Create new internal node", result[[i]]$new_node, "\n")
    }
    cat("\n")
  }
  
  return(result)
}

# Run the solution
solution <- solve_neighbor_joining()
```

This implementation follows the neighbor joining algorithm steps:

1. **Input**: A distance matrix representing pairwise distances between taxa
2. **Process**: 
   - Calculate the Q matrix for each pair of nodes
   - Find the minimum Q value to determine which nodes to join
   - Calculate branch lengths using the neighbor joining formula
   - Create a new internal node and update the distance matrix
   - Repeat until only 2 nodes remain
3. **Output**: A tree structure showing the joining process

The algorithm has time complexity O(n³) and space complexity O(n²) where n is the number of taxa. The key steps involve:
- Computing the Q matrix using the formula: Q[i,j] = (n-2)d[i,j] - Σd[i,k] - Σd[j,k]
- Calculating branch lengths using the neighbor joining formula
- Updating the distance matrix after each join operation

The output shows each step of the joining process with the corresponding branch lengths and node connections.

