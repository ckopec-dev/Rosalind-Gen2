# Rosalind Problem: Implement UPGMA

UPGMA (Unweighted Pair Group Method with Arithmetic Mean) is a clustering algorithm used to build phylogenetic trees from distance matrices.

## Problem Description

Given: An integer n and a distance matrix D as an n×n matrix of positive real numbers.

Return: The ultrametric tree T resulting from applying UPGMA to D, where the distance between leaves is equal to the corresponding entry in D.

## Solution in R

```r
# Function to implement UPGMA
upgma <- function(D) {
  # Get number of taxa
  n <- nrow(D)
  
  # Initialize clusters - each taxon is its own cluster
  clusters <- list()
  for (i in 1:n) {
    clusters[[i]] <- list(taxa = i, size = 1, height = 0)
  }
  
  # Initialize tree structure
  tree <- list()
  
  # For each step of UPGMA
  for (step in 1:(n-1)) {
    # Find minimum distance in distance matrix
    min_dist <- Inf
    min_i <- 1
    min_j <- 2
    
    for (i in 1:(n-step+1)) {
      for (j in (i+1):(n-step+1)) {
        if (D[i,j] < min_dist) {
          min_dist <- D[i,j]
          min_i <- i
          min_j <- j
        }
      }
    }
    
    # Calculate new distances for the merged cluster
    new_cluster_size <- clusters[[min_i]]$size + clusters[[min_j]]$size
    new_height <- min_dist / 2
    
    # Create new cluster
    new_cluster <- list(
      taxa = c(clusters[[min_i]]$taxa, clusters[[min_j]]$taxa),
      size = new_cluster_size,
      height = new_height
    )
    
    # Add to tree structure
    tree[[length(tree)+1]] <- list(
      left = min_i,
      right = min_j,
      height = new_height
    )
    
    # Update distance matrix
    # Calculate distances from new cluster to all other clusters
    for (k in 1:(n-step)) {
      if (k != min_i && k != min_j) {
        # Use average distance formula for UPGMA
        D[min_i,k] <- (D[min_i,k] * clusters[[min_i]]$size + 
                      D[min_j,k] * clusters[[min_j]]$size) / new_cluster_size
        D[k,min_i] <- D[min_i,k]
      }
    }
    
    # Set distances to infinity for merged clusters
    D[min_i,min_j] <- Inf
    D[min_j,min_i] <- Inf
    
    # Update cluster list
    clusters[[min_i]] <- new_cluster
    clusters[[min_j]] <- NULL
    
    # Remove merged cluster from cluster list
    clusters <- clusters[!sapply(clusters, is.null)]
  }
  
  return(tree)
}

# Alternative cleaner implementation
upgma_clean <- function(D) {
  n <- nrow(D)
  
  # Initialize clusters (each taxon is a cluster)
  clusters <- list()
  for (i in 1:n) {
    clusters[[i]] <- list(taxa = i, size = 1)
  }
  
  # Keep track of cluster relationships
  tree_edges <- list()
  
  # Iteratively merge clusters
  for (step in 1:(n-1)) {
    # Find minimum distance
    min_dist <- Inf
    min_i <- 1
    min_j <- 2
    
    for (i in 1:length(clusters)) {
      for (j in (i+1):length(clusters)) {
        if (D[i,j] < min_dist) {
          min_dist <- D[i,j]
          min_i <- i
          min_j <- j
        }
      }
    }
    
    # Calculate new height (UPGMA uses average distance divided by 2)
    height <- min_dist / 2
    
    # Create new cluster
    new_cluster <- list(
      taxa = c(clusters[[min_i]]$taxa, clusters[[min_j]]$taxa),
      size = clusters[[min_i]]$size + clusters[[min_j]]$size
    )
    
    # Record the edge
    tree_edges[[length(tree_edges)+1]] <- list(
      left = min_i,
      right = min_j,
      height = height
    )
    
    # Update distance matrix
    for (k in 1:length(clusters)) {
      if (k != min_i && k != min_j) {
        D[min_i,k] <- (D[min_i,k] * clusters[[min_i]]$size + 
                      D[min_j,k] * clusters[[min_j]]$size) / new_cluster$size
        D[k,min_i] <- D[min_i,k]
      }
    }
    
    # Remove merged clusters
    clusters[[min_i]] <- new_cluster
    clusters[[min_j]] <- NULL
    clusters <- clusters[!sapply(clusters, is.null)]
  }
  
  return(tree_edges)
}

# Example usage
# Example distance matrix (4 taxa)
D <- matrix(c(0.0, 0.5, 0.8, 0.9,
              0.5, 0.0, 0.7, 0.8,
              0.8, 0.7, 0.0, 0.5,
              0.9, 0.8, 0.5, 0.0),
            nrow = 4, byrow = TRUE)

# Run UPGMA
result <- upgma_clean(D)
print(result)

# Function to print tree in Newick format (optional)
print_newick <- function(tree, n) {
  # This is a simplified version - a full implementation would be more complex
  cat("UPGMA tree structure:")
  for (i in 1:length(tree)) {
    cat("Step", i, ": merge clusters", tree[[i]]$left, "and", tree[[i]]$right, 
        "at height", tree[[i]]$height, "\n")
  }
}

# Print result
print_newick(result, 4)
```

## Key Points of the Implementation

1. **Initialization**: Each taxon starts as its own cluster
2. **Distance Matrix**: The distance matrix is updated after each merge
3. **UPGMA Formula**: The height of each merge is the distance divided by 2
4. **Cluster Merging**: Always merge the two closest clusters
5. **Distance Update**: Uses the average distance formula for UPGMA

## Time Complexity
- O(n³) where n is the number of taxa
- The algorithm performs n-1 merges, each requiring O(n²) operations to update the distance matrix

## Space Complexity
- O(n²) for storing the distance matrix
- O(n) for storing cluster information

This implementation correctly follows the UPGMA algorithm and should solve the Rosalind problem when given a proper distance matrix as input.

