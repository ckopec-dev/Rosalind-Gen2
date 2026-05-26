# Rosalind Problem: Generate the Last-to-First Mapping of a String

## Problem Description
Given a string Text, we can form a new string by sorting all the characters in Text in alphabetical order. We can then create a mapping from the last positions to the first positions of the sorted characters.

## Solution in R

```r
# Function to generate last-to-first mapping
last_to_first_mapping <- function(text) {
  # Add sentinel character to handle circular permutation
  text <- paste0(text, "$")
  
  # Create all rotations of the text
  n <- nchar(text)
  rotations <- character(n)
  
  for (i in 1:n) {
    rotations[i] <- substr(text, i, nchar(text))
    if (i > 1) {
      rotations[i] <- paste0(rotations[i], substr(text, 1, i-1))
    }
  }
  
  # Sort rotations lexicographically
  sorted_rotations <- sort(rotations)
  
  # Create mapping from last column to first column
  last_col <- sapply(sorted_rotations, function(x) substr(x, n, n))
  first_col <- sapply(sorted_rotations, function(x) substr(x, 1, 1))
  
  # Create mapping: last position -> first position
  mapping <- numeric(n)
  for (i in 1:n) {
    pos <- which(last_col == substr(text, i, i))
    if (length(pos) > 0) {
      mapping[i] <- pos[1]
    }
  }
  
  return(mapping)
}

# Alternative cleaner approach
generate_last_to_first <- function(text) {
  # Add sentinel character
  text <- paste0(text, "$")
  
  # Create all rotations
  n <- nchar(text)
  rotations <- character(n)
  
  for (i in 1:n) {
    rotations[i] <- substr(text, i, nchar(text))
    if (i > 1) {
      rotations[i] <- paste0(rotations[i], substr(text, 1, i-1))
    }
  }
  
  # Sort rotations
  sorted_indices <- order(rotations)
  
  # Create mapping: for each position in original text, 
  # find its position in the sorted rotations
  mapping <- numeric(n)
  
  for (i in 1:n) {
    # Find where character at position i appears in sorted rotations
    char_at_i <- substr(text, i, i)
    for (j in 1:n) {
      if (substr(rotations[sorted_indices[j]], 1, 1) == char_at_i) {
        mapping[i] <- j
        break
      }
    }
  }
  
  return(mapping)
}

# More efficient approach using Burrows-Wheeler Transform concepts
bwt_last_to_first <- function(text) {
  # Add sentinel character
  text <- paste0(text, "$")
  
  # Create BWT matrix
  n <- nchar(text)
  bwt_matrix <- matrix(nrow = n, ncol = n)
  
  # Fill the matrix with rotations
  for (i in 1:n) {
    rotation <- substr(text, i, nchar(text))
    if (i > 1) {
      rotation <- paste0(rotation, substr(text, 1, i-1))
    }
    bwt_matrix[i, ] <- strsplit(rotation, "")[[1]]
  }
  
  # Sort rows lexicographically
  sorted_indices <- order(apply(bwt_matrix, 1, paste, collapse = ""))
  
  # Extract first and last columns
  first_col <- apply(bwt_matrix[sorted_indices, ], 2, paste, collapse = "")
  last_col <- apply(bwt_matrix[sorted_indices, ], 2, paste, collapse = "")
  
  # Create mapping from last to first positions
  mapping <- numeric(n)
  for (i in 1:n) {
    # Find position of last_col[i] in first_col
    for (j in 1:n) {
      if (substr(first_col, j, j) == substr(last_col, i, i)) {
        mapping[i] <- j
        break
      }
    }
  }
  
  return(mapping)
}

# Simpler direct approach
last_to_first_simple <- function(text) {
  # Add sentinel
  text <- paste0(text, "$")
  
  # Create sorted rotations
  n <- nchar(text)
  rotations <- character(n)
  
  for (i in 1:n) {
    if (i == 1) {
      rotations[i] <- text
    } else {
      rotations[i] <- paste0(substr(text, i, n), substr(text, 1, i-1))
    }
  }
  
  # Sort rotations
  sorted_rotations <- sort(rotations)
  
  # Create mapping from original text positions to sorted positions
  mapping <- numeric(n)
  for (i in 1:n) {
    char_at_i <- substr(text, i, i)
    # Find which position in sorted rotations has this character at first position
    for (j in 1:n) {
      if (substr(sorted_rotations[j], 1, 1) == char_at_i) {
        mapping[i] <- j
        break
      }
    }
  }
  
  return(mapping)
}

# Example usage
text <- "ABRACADABRA"
result <- last_to_first_simple(text)
print(paste("Text:", text))
print(paste("Last-to-First mapping:", paste(result, collapse = " ")))

# Test with a simple example
text2 <- "ACGT"
result2 <- last_to_first_simple(text2)
print(paste("Text:", text2))
print(paste("Last-to-First mapping:", paste(result2, collapse = " ")))
```

## Explanation

This problem involves creating a mapping between positions in a string and their corresponding positions in the sorted rotations of that string. The approach:

1. **Add a sentinel character** (`$`) to handle the circular nature of rotations
2. **Generate all rotations** of the string
3. **Sort these rotations** lexicographically 
4. **Create the mapping** from last column positions to first column positions

The mapping is particularly useful in the Burrows-Wheeler Transform (BWT) and related algorithms for string compression and bioinformatics applications.

## Time Complexity
- O(n² log n) where n is the length of the text, due to sorting rotations

## Space Complexity  
- O(n²) for storing all rotations

The function returns a vector where element i represents the position in the sorted rotations that corresponds to the character at position i in the original text.

