# Rosalind Problem: Implement_Hierarchical_Clustering

## Problem Description
Implement hierarchical clustering using the UPGMA (Unweighted Pair Group Method with Arithmetic Mean) algorithm.

## Solution in R

```r
# Function to implement hierarchical clustering using UPGMA
hierarchical_clustering <- function(distances) {
  # Convert distance matrix to a list of distances
  n <- nrow(distances)
  
  # Initialize clusters
  clusters <- list()
  for (i in 1:n) {
    clusters[[i]] <- list(name = i, members = c(i))
  }
  
  # Initialize result list
  result <- list()
  
  # While we have more than one cluster
  while (length(clusters) > 1) {
    # Find minimum distance between clusters
    min_dist <- Inf
    min_i <- 1
    min_j <- 2
    
    for (i in 1:(length(clusters) - 1)) {
      for (j in (i + 1):length(clusters)) {
        # Calculate average distance between clusters
        dist <- 0
        count <- 0
        
        for (k in clusters[[i]]$members) {
          for (l in clusters[[j]]$members) {
            dist <- dist + distances[k, l]
            count <- count + 1
          }
        }
        
        avg_dist <- dist / count
        
        if (avg_dist < min_dist) {
          min_dist <- avg_dist
          min_i <- i
          min_j <- j
        }
      }
    }
    
    # Create new cluster
    new_cluster <- list(
      name = paste0("C", length(result) + 1),
      members = c(clusters[[min_i]]$members, clusters[[min_j]]$members)
    )
    
    # Add to result
    result[[length(result) + 1]] <- list(
      cluster1 = clusters[[min_i]]$name,
      cluster2 = clusters[[min_j]]$name,
      distance = min_dist
    )
    
    # Remove old clusters and add new one
    clusters[[min_i]] <- new_cluster
    clusters <- clusters[-min_j]
    
    # Re-index clusters
    for (i in 1:length(clusters)) {
      clusters[[i]]$name <- i
    }
  }
  
  return(result)
}

# Alternative more efficient implementation
upgma <- function(dist_matrix) {
  n <- nrow(dist_matrix)
  
  # Initialize cluster information
  clusters <- list()
  for (i in 1:n) {
    clusters[[i]] <- list(
      id = i,
      members = c(i),
      size = 1
    )
  }
  
  # Initialize result
  result <- list()
  
  # Keep track of cluster sizes for distance calculations
  cluster_sizes <- rep(1, n)
  
  # While we have more than one cluster
  while (length(clusters) > 1) {
    # Find minimum distance
    min_dist <- Inf
    min_i <- 1
    min_j <- 2
    
    for (i in 1:(length(clusters) - 1)) {
      for (j in (i + 1):length(clusters)) {
        # Calculate average distance between clusters
        dist <- 0
        count <- 0
        
        for (k in clusters[[i]]$members) {
          for (l in clusters[[j]]$members) {
            dist <- dist + dist_matrix[k, l]
            count <- count + 1
          }
        }
        
        avg_dist <- dist / count
        
        if (avg_dist < min_dist) {
          min_dist <- avg_dist
          min_i <- i
          min_j <- j
        }
      }
    }
    
    # Create new cluster
    new_members <- c(clusters[[min_i]]$members, clusters[[min_j]]$members)
    new_size <- clusters[[min_i]]$size + clusters[[min_j]]$size
    
    # Add to result
    result[[length(result) + 1]] <- list(
      cluster1 = clusters[[min_i]]$id,
      cluster2 = clusters[[min_j]]$id,
      distance = min_dist
    )
    
    # Remove old clusters and add new one
    clusters[[min_i]] <- list(
      id = length(result),
      members = new_members,
      size = new_size
    )
    
    # Remove second cluster
    clusters <- clusters[-min_j]
  }
  
  return(result)
}

# Example usage with sample data
# Sample distance matrix
sample_distances <- matrix(c(
  0.0, 1.0, 2.0, 3.0,
  1.0, 0.0, 1.0, 2.0,
  2.0, 1.0, 0.0, 1.0,
  3.0, 2.0, 1.0, 0.0
), nrow = 4, byrow = TRUE)

# Run clustering
clustering_result <- upgma(sample_distances)

# Print results
cat("Hierarchical Clustering Result:\n")
for (i in 1:length(clustering_result)) {
  cat(sprintf("Step %d: Merge clusters %d and %d (distance: %.1f)\n", 
              i, 
              clustering_result[[i]]$cluster1, 
              clustering_result[[i]]$cluster2, 
              clustering_result[[i]]$distance))
}
```

## Key Features of the Implementation

1. **UPGMA Algorithm**: Implements the Unweighted Pair Group Method with Arithmetic Mean
2. **Cluster Merging**: Continuously merges the closest clusters based on average distances
3. **Distance Calculation**: Computes average distances between clusters
4. **Result Tracking**: Records each merge operation with corresponding distance

## How It Works

1. Start with each element as its own cluster
2. Find the pair of clusters with minimum average distance
3. Merge these clusters into a new cluster
4. Update distance matrix for the new cluster
5. Repeat until only one cluster remains

## Time Complexity
- O(n³) where n is the number of elements
- Due to repeated distance calculations and cluster merging operations

## Space Complexity
- O(n²) for storing the distance matrix
- O(n) for cluster information storage

This implementation follows the standard UPGMA algorithm and should work correctly for the Rosalind problem requirements.

