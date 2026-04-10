# Rosalind Problem: Alignment-Based_Phylogeny

## Problem Description

Given a collection of DNA strings, construct a phylogenetic tree using the neighbor-joining algorithm based on sequence alignment.

## Solution in R

```r
# Load required libraries
library(seqinr)
library(ape)

# Function to calculate genetic distance between two sequences
genetic_distance <- function(seq1, seq2) {
  # Convert to character vectors
  s1 <- unlist(strsplit(seq1, ""))
  s2 <- unlist(strsplit(seq2, ""))
  
  # Calculate number of differences
  diff_count <- sum(s1 != s2)
  
  # Return distance (proportion of differences)
  return(diff_count / length(s1))
}

# Function to create distance matrix
create_distance_matrix <- function(sequences) {
  n <- length(sequences)
  dist_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(dist_matrix) <- names(sequences)
  colnames(dist_matrix) <- names(sequences)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        dist_matrix[i, j] <- 0
      } else {
        dist_matrix[i, j] <- genetic_distance(sequences[i], sequences[j])
      }
    }
  }
  
  return(dist_matrix)
}

# Function to find minimum element in distance matrix
find_min_element <- function(dist_matrix) {
  n <- nrow(dist_matrix)
  min_val <- Inf
  min_i <- 0
  min_j <- 0
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && dist_matrix[i, j] < min_val) {
        min_val <- dist_matrix[i, j]
        min_i <- i
        min_j <- j
      }
    }
  }
  
  return(list(i = min_i, j = min_j, distance = min_val))
}

# Function to compute Q matrix
compute_Q_matrix <- function(dist_matrix) {
  n <- nrow(dist_matrix)
  Q_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(Q_matrix) <- rownames(dist_matrix)
  colnames(Q_matrix) <- colnames(dist_matrix)
  
  # Calculate row sums
  row_sums <- apply(dist_matrix, 1, sum)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        Q_matrix[i, j] <- (n - 2) * dist_matrix[i, j] - 
                         row_sums[i] - row_sums[j]
      }
    }
  }
  
  return(Q_matrix)
}

# Function to perform neighbor joining
neighbor_joining <- function(sequences) {
  # Create initial distance matrix
  dist_matrix <- create_distance_matrix(sequences)
  
  # Create list of nodes
  nodes <- names(sequences)
  n <- length(nodes)
  
  # Initialize tree structure
  tree <- list()
  
  # Keep joining until we have only one node
  while (length(nodes) > 2) {
    # Compute Q matrix
    Q_matrix <- compute_Q_matrix(dist_matrix)
    
    # Find minimum element in Q matrix
    min_element <- find_min_element(Q_matrix)
    i <- min_element$i
    j <- min_element$j
    
    # Calculate branch lengths
    distance_ij <- min_element$distance
    n_nodes <- length(nodes)
    
    # Calculate branch lengths to new node
    if (n_nodes > 2) {
      branch_i <- (dist_matrix[i, j] + 
                  (sum(dist_matrix[i, ]) - sum(dist_matrix[j, ])) / (n_nodes - 2)) / 2
      branch_j <- dist_matrix[i, j] - branch_i
    } else {
      branch_i <- distance_ij / 2
      branch_j <- distance_ij / 2
    }
    
    # Create new node
    new_node_name <- paste0("Node_", length(nodes))
    
    # Add new node to tree
    tree[[length(tree) + 1]] <- list(
      node1 = nodes[i],
      node2 = nodes[j],
      branch1 = branch_i,
      branch2 = branch_j,
      new_node = new_node_name
    )
    
    # Update distance matrix
    new_distances <- numeric(n_nodes)
    
    for (k in 1:n_nodes) {
      if (k != i && k != j) {
        new_distances[k] <- (dist_matrix[i, k] + dist_matrix[j, k] - dist_matrix[i, j]) / 2
      }
    }
    
    # Remove old nodes and add new one
    nodes <- nodes[-c(i, j)]
    nodes <- c(nodes, new_node_name)
    
    # Update distance matrix
    dist_matrix <- dist_matrix[-c(i, j), -c(i, j)]
    
    # Add new row/column for the new node
    new_row <- c(new_distances, 0)
    dist_matrix <- rbind(dist_matrix, new_row)
    new_col <- c(new_row[-length(new_row)], 0)
    dist_matrix <- cbind(dist_matrix, new_col)
    
    # Adjust indices to account for removed nodes
    if (i < j) {
      i <- i
      j <- j - 1
    } else {
      i <- i - 1
      j <- j
    }
  }
  
  # Final two nodes
  tree[[length(tree) + 1]] <- list(
    node1 = nodes[1],
    node2 = nodes[2],
    branch1 = dist_matrix[1, 2] / 2,
    branch2 = dist_matrix[1, 2] / 2,
    new_node = "root"
  )
  
  return(tree)
}

# Main function to solve the problem
solve_alignment_phylogeny <- function(input_file = NULL, sequences = NULL) {
  if (!is.null(input_file)) {
    # Read sequences from file
    seqs <- read.fasta(input_file, forceDNAtolower = FALSE)
    sequences <- sapply(seqs, function(x) paste(x, collapse = ""))
    names(sequences) <- names(seqs)
  }
  
  # Perform neighbor joining
  tree <- neighbor_joining(sequences)
  
  # Print results
  cat("Phylogenetic tree constructed using neighbor-joining algorithm:\n")
  for (i in 1:length(tree)) {
    cat("Step", i, ":\n")
    cat("  Node 1:", tree[[i]]$node1, "Branch length:", tree[[i]]$branch1, "\n")
    cat("  Node 2:", tree[[i]]$node2, "Branch length:", tree[[i]]$branch2, "\n")
    cat("  New node:", tree[[i]]$new_node, "\n\n")
  }
  
  return(tree)
}

# Example usage with sample data
# Sample DNA sequences
sample_sequences <- c(
  "ATCGATCG",
  "ATCGATCG",
  "ATCGATCG",
  "ATCGATCG"
)

# Set names for sequences
names(sample_sequences) <- c("Seq1", "Seq2", "Seq3", "Seq4")

# Solve the problem
result <- solve_alignment_phylogeny(sequences = sample_sequences)

# For a more realistic example with different sequences:
realistic_sequences <- c(
  "ATCGATCG",
  "ATCGATCG",
  "ATCGATCG",
  "ATCGATCG"
)

names(realistic_sequences) <- c("Species_A", "Species_B", "Species_C", "Species_D")

# Solve with realistic sequences
realistic_result <- solve_alignment_phylogeny(sequences = realistic_sequences)

# Alternative approach using built-in R packages
# This is a more practical approach for real data
create_phylogeny <- function(sequences) {
  # Convert to DNAStringSet
  library(Biostrings)
  
  # Create DNAStringSet
  dna_strings <- DNAStringSet(sequences)
  names(dna_strings) <- names(sequences)
  
  # Create distance matrix
  dist_matrix <- distDna(dna_strings, model = "raw")
  
  # Convert to matrix for easier handling
  dist_matrix <- as.matrix(dist_matrix)
  
  # Use ape package for neighbor joining
  tree <- nj(dist_matrix)
  
  return(tree)
}

# Example with actual distance matrix
example_sequences <- c(
  "ACGTACGT",
  "ACGTACGT",
  "ACGTACGT",
  "ACGTACGT"
)

names(example_sequences) <- c("A", "B", "C", "D")

# Create distance matrix manually for demonstration
example_dist_matrix <- matrix(c(
  0.0, 0.2, 0.3, 0.4,
  0.2, 0.0, 0.3, 0.4,
  0.3, 0.3, 0.0, 0.2,
  0.4, 0.4, 0.2, 0.0
), nrow = 4, byrow = TRUE)

rownames(example_dist_matrix) <- c("A", "B", "C", "D")
colnames(example_dist_matrix) <- c("A", "B", "C", "D")

# Create phylogenetic tree using ape
library(ape)
tree <- nj(example_dist_matrix)
print(tree)
plot(tree, type = "p", cex = 0.8)
```

## Explanation

This solution implements the neighbor-joining algorithm for constructing phylogenetic trees from DNA sequence data:

1. **Distance Calculation**: Computes genetic distances between sequences using the proportion of differences
2. **Q Matrix Computation**: Calculates the Q matrix to identify the next pair of nodes to join
3. **Neighbor Joining**: Repeatedly joins the closest pairs of nodes until a complete tree is formed
4. **Tree Construction**: Builds the final phylogenetic tree with branch lengths

## Key Functions

- `genetic_distance()`: Calculates the genetic distance between two sequences
- `create_distance_matrix()`: Creates a distance matrix from a set of sequences
- `neighbor_joining()`: Implements the neighbor-joining algorithm
- `solve_alignment_phylogeny()`: Main function to solve the problem

## Usage

```r
# For file input
result <- solve_alignment_phylogeny(input_file = "sequences.fasta")

# For direct sequence input
sequences <- c("ATCGATCG", "ATCGATCG", "ATCGATCG")
names(sequences) <- c("Seq1", "Seq2", "Seq3")
result <- solve_alignment_phylogeny(sequences = sequences)
```

The solution handles the core requirements of building a phylogenetic tree from DNA sequences using the neighbor-joining method, which is a standard approach for this type of bioinformatics problem.

