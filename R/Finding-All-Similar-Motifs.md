# Rosalind Problem: Finding All Similar Motifs

## Problem Description
Given two DNA strings s and t, find all locations where t occurs as a substring of s.

## Solution in R

```r
# Function to find all occurrences of a motif in a DNA string
find_motif_locations <- function(s, t) {
  # Convert strings to character vectors
  s_chars <- unlist(strsplit(s, ""))
  t_chars <- unlist(strsplit(t, ""))
  
  # Get lengths
  s_len <- length(s_chars)
  t_len <- length(t_chars)
  
  # Vector to store locations
  locations <- c()
  
  # Check each possible position
  for (i in 1:(s_len - t_len + 1)) {
    # Extract substring of length t_len starting at position i
    substring_chars <- s_chars[i:(i + t_len - 1)]
    
    # Compare with motif
    if (identical(substring_chars, t_chars)) {
      locations <- c(locations, i)
    }
  }
  
  return(locations)
}

# Alternative more efficient approach using regular expressions
find_motif_locations_regex <- function(s, t) {
  # Escape special regex characters in motif
  t_escaped <- gsub("([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", t)
  
  # Find all matches
  matches <- gregexpr(t_escaped, s, perl = TRUE)
  
  # Extract positions (add 1 because R is 1-indexed)
  positions <- as.vector(matches[[1]])
  
  # Filter out -1 (no match) and return as 1-indexed
  return(positions[positions > 0])
}

# Read input from file (assuming input format: first line = s, second line = t)
read_input <- function(filename) {
  lines <- readLines(filename)
  s <- lines[1]
  t <- lines[2]
  return(list(s = s, t = t))
}

# Main execution
# Example usage with sample data
s <- "GATATATGCATATACTT"
t <- "ATAT"

# Find all motif locations
locations <- find_motif_locations(s, t)
print(paste("Locations:", paste(locations, collapse = " ")))

# Using regex approach
locations_regex <- find_motif_locations_regex(s, t)
print(paste("Locations (regex):", paste(locations_regex, collapse = " ")))

# For actual Rosalind submission, process input file
# input <- read_input("rosalind_substring.txt")
# result <- find_motif_locations(input$s, input$t)
# write(paste(result, collapse = " "), "output.txt")
```

## Explanation

The problem asks us to find all starting positions where a motif (substring `t`) appears in a DNA string (`s`).

### Approach 1: Manual Comparison
1. Convert both strings to character vectors
2. Iterate through each possible starting position in `s`
3. Extract substring of length `t` at each position
4. Compare with the motif `t`
5. Record positions where matches occur

### Approach 2: Regular Expressions
1. Use `gregexpr()` function to find all matches
2. The function returns positions of all occurrences
3. Convert to 1-indexed positions (R uses 1-based indexing)

## Sample Input/Output

**Input:**
```
GATATATGCATATACTT
ATAT
```

**Output:**
```
2 4 10
```

## Key Points

- R uses 1-based indexing, so positions are 1-indexed (not 0-indexed like in some other languages)
- The `gregexpr()` approach is more efficient for large inputs
- Handle edge cases like when the motif is longer than the sequence
- Multiple matches can occur at different positions

## For Rosalind Submission

```r
# Complete solution for Rosalind submission
# Read from stdin or file
s <- readLines("rosalind_substring.txt")[1]
t <- readLines("rosalind_substring.txt")[2]

# Find all locations
locations <- find_motif_locations_regex(s, t)

# Output results (1-indexed)
cat(paste(locations, collapse = " "), "\n")
```

This solution efficiently finds all occurrences of a motif in a DNA sequence and returns their 1-indexed positions.

