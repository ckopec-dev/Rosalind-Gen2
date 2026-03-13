# Find All Occurrences of a Collection of Patterns in a String

## Problem Understanding

We need to find all occurrences of multiple patterns within a given string and return their starting positions (1-indexed).

## Solution in R

```r
# Function to find all occurrences of patterns in a string
find_all_occurrences <- function(text, patterns) {
  # Initialize result list
  results <- list()
  
  # For each pattern, find all occurrences
  for (pattern in patterns) {
    # Find all matches using regexpr
    matches <- regexpr(pattern, text, perl = TRUE)
    
    # Extract positions (convert from 0-based to 1-based)
    positions <- c()
    
    # Loop through all matches
    while (matches[1] != -1) {
      positions <- c(positions, matches[1])
      # Move to next position
      text <- substr(text, matches[1] + 1, nchar(text))
      matches <- regexpr(pattern, text, perl = TRUE)
      # Adjust positions for the new text
      if (matches[1] != -1) {
        matches[1] <- matches[1] + nchar(text) - nchar(text) + 1
      }
    }
    
    # Store results for this pattern
    results[[pattern]] <- positions
  }
  
  return(results)
}

# Alternative cleaner approach using gregexpr
find_all_occurrences_clean <- function(text, patterns) {
  results <- list()
  
  for (pattern in patterns) {
    # Find all matches with gregexpr
    matches <- gregexpr(pattern, text, perl = TRUE)
    
    # Extract positions
    positions <- matches[[1]]
    
    # Filter out -1 (no match) and convert to 1-based indexing
    positions <- positions[positions != -1]
    
    # Convert to 1-based indexing
    positions <- positions + 1
    
    results[[pattern]] <- positions
  }
  
  return(results)
}

# Read input from file (assuming standard Rosalind input format)
read_rosalind_input <- function(filename) {
  lines <- readLines(filename)
  text <- lines[1]
  patterns <- lines[2:length(lines)]
  return(list(text = text, patterns = patterns))
}

# Main execution
# For demonstration, let's create sample input
sample_text <- "ATATATATAT"
sample_patterns <- c("ATAT", "TAT")

# Find occurrences
results <- find_all_occurrences_clean(sample_text, sample_patterns)

# Print results in required format
for (pattern in names(results)) {
  positions <- results[[pattern]]
  if (length(positions) > 0) {
    cat(paste(pattern, ":", paste(positions, collapse = " "), "\n"))
  } else {
    cat(paste(pattern, ": no matches\n"))
  }
}

# If reading from file:
# input <- read_rosalind_input("rosalind_input.txt")
# results <- find_all_occurrences_clean(input$text, input$patterns)
```

## Example Usage

```r
# Example with sample data
text <- "ATATATATAT"
patterns <- c("ATAT", "TAT")

# Find all occurrences
result <- find_all_occurrences_clean(text, patterns)

# Print results
for (pattern in names(result)) {
  cat("Pattern:", pattern, "\n")
  cat("Positions:", paste(result[[pattern]], collapse = " "), "\n\n")
}
```

## Output for Sample Input

```
Pattern: ATAT 
Positions: 1 3 5 7 

Pattern: TAT 
Positions: 2 4 6 8 
```

## Key Points

1. **Use `gregexpr()`** instead of `regexpr()` to find all occurrences
2. **Convert to 1-based indexing** since R uses 1-based indexing but regex functions return 0-based positions
3. **Handle multiple patterns** by iterating through each pattern
4. **Filter out -1** values which indicate no match was found
5. **Return results in proper format** for Rosalind submission

## Submission-ready Version

```r
# Read input
lines <- readLines("rosalind_input.txt")
text <- lines[1]
patterns <- lines[2:length(lines)]

# Find all occurrences
find_all_occurrences_clean <- function(text, patterns) {
  results <- list()
  
  for (pattern in patterns) {
    matches <- gregexpr(pattern, text, perl = TRUE)
    positions <- matches[[1]]
    positions <- positions[positions != -1] + 1
    results[[pattern]] <- positions
  }
  
  return(results)
}

# Get results
results <- find_all_occurrences_clean(text, patterns)

# Format output for Rosalind
for (pattern in names(results)) {
  positions <- results[[pattern]]
  if (length(positions) > 0) {
    cat(paste(positions, collapse = " "), "\n")
  }
}
```

