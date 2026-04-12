# Rosalind Problem: Phylogeny Comparison with Split Distance

## Problem Description

Given two trees, compute the split distance between them. The split distance is the number of splits (edges) that are present in one tree but not in the other.

## Solution in R

```r
# Load required libraries
library(ape)
library(phangorn)

# Function to compute split distance between two trees
split_distance <- function(tree1, tree2) {
  # Convert trees to phylo objects if they aren't already
  if (!inherits(tree1, "phylo")) {
    tree1 <- read.tree(text = tree1)
  }
  if (!inherits(tree2, "phylo")) {
    tree2 <- read.tree(text = tree2)
  }
  
  # Get splits from both trees
  splits1 <- split.tree(tree1)
  splits2 <- split.tree(tree2)
  
  # Count splits in tree1 that are not in tree2
  unique_to_tree1 <- sum(!splits1 %in% splits2)
  
  # Count splits in tree2 that are not in tree1
  unique_to_tree2 <- sum(!splits2 %in% splits1)
  
  # Return total split distance
  return(unique_to_tree1 + unique_to_tree2)
}

# Alternative implementation using phangorn package
split_distance_phangorn <- function(tree1, tree2) {
  # Read trees using phangorn
  if (!inherits(tree1, "phylo")) {
    tree1 <- read.tree(text = tree1)
  }
  if (!inherits(tree2, "phylo")) {
    tree2 <- read.tree(text = tree2)
  }
  
  # Use the splitDistance function from phangorn
  return(splitDistance(tree1, tree2))
}

# Example usage
# Example trees (replace with actual input)
tree1_text <- "((A,B),(C,D));"
tree2_text <- "((A,C),(B,D));"

# Method 1: Manual implementation
distance1 <- split_distance(tree1_text, tree2_text)
print(paste("Split distance (manual):", distance1))

# Method 2: Using phangorn
distance2 <- split_distance_phangorn(tree1_text, tree2_text)
print(paste("Split distance (phangorn):", distance2))

# Function to handle multiple tree pairs from input file
solve_phylogeny_comparison <- function(input_file) {
  # Read input file
  lines <- readLines(input_file)
  
  # Parse trees from input
  trees <- list()
  i <- 1
  while (i <= length(lines)) {
    if (lines[i] == "") {
      i <- i + 1
      next
    }
    tree1 <- lines[i]
    tree2 <- lines[i + 1]
    trees[[length(trees) + 1]] <- list(tree1 = tree1, tree2 = tree2)
    i <- i + 2
  }
  
  # Calculate split distances
  results <- c()
  for (pair in trees) {
    distance <- split_distance_phangorn(pair$tree1, pair$tree2)
    results <- c(results, distance)
  }
  
  return(results)
}

# For a single pair of trees in text format
compute_split_distance <- function(tree1_text, tree2_text) {
  # Read trees
  tree1 <- read.tree(text = tree1_text)
  tree2 <- read.tree(text = tree2_text)
  
  # Calculate split distance using phangorn
  distance <- splitDistance(tree1, tree2)
  
  return(distance)
}

# Example with specific input
# Sample input trees
tree1 <- "((A,B),(C,D));"
tree2 <- "((A,C),(B,D));"

# Compute split distance
result <- compute_split_distance(tree1, tree2)
print(paste("Split distance:", result))
```

## Key Points

1. **Split Distance**: The number of splits (edges) that are present in one tree but not in the other
2. **Implementation**: Uses the `phangorn` package's `splitDistance()` function for accuracy
3. **Input Format**: Trees should be in Newick format
4. **Output**: Single integer representing the split distance

## Usage

```r
# For a single pair of trees
tree1 <- "((A,B),(C,D));"
tree2 <- "((A,C),(B,D));"
distance <- compute_split_distance(tree1, tree2)
print(distance)
```

The solution handles tree parsing, split computation, and distance calculation efficiently using R's bioinformatics packages.

