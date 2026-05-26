# Rosalind Problem: Overlap Graphs (Overlap_Graphs)

## Problem Description
Given a collection of DNA strings in FASTA format, construct a directed graph where each string is a node, and there's a directed edge from string A to string B if the suffix of A (of length k) matches the prefix of B (of length k), where k is a given overlap length.

## Solution in R

```r
# Function to solve Overlap Graphs problem
overlap_graphs <- function(fasta_file, k = 3) {
  # Read FASTA file
  fasta_content <- readLines(fasta_file)
  
  # Parse FASTA sequences
  sequences <- list()
  current_id <- ""
  
  for (line in fasta_content) {
    if (grepl("^>", line)) {
      # New sequence ID
      current_id <- gsub("^>", "", line)
      sequences[[current_id]] <- ""
    } else {
      # Sequence data
      if (length(sequences[[current_id]]) == 0) {
        sequences[[current_id]] <- line
      } else {
        sequences[[current_id]] <- paste0(sequences[[current_id]], line)
      }
    }
  }
  
  # Convert to data frame for easier handling
  ids <- names(sequences)
  seqs <- unlist(sequences)
  df <- data.frame(id = ids, sequence = seqs, stringsAsFactors = FALSE)
  
  # Find overlaps
  result <- c()
  
  for (i in 1:nrow(df)) {
    for (j in 1:nrow(df)) {
      if (i != j) {
        # Check if suffix of df[i] matches prefix of df[j]
        suffix <- substr(df$sequence[i], nchar(df$sequence[i]) - k + 1, nchar(df$sequence[i]))
        prefix <- substr(df$sequence[j], 1, k)
        
        if (suffix == prefix) {
          result <- c(result, paste0(df$id[i], " ", df$id[j]))
        }
      }
    }
  }
  
  return(result)
}

# Alternative more efficient approach using string operations
overlap_graphs_efficient <- function(fasta_file, k = 3) {
  # Read FASTA file
  fasta_content <- readLines(fasta_file)
  
  # Parse FASTA sequences
  sequences <- list()
  current_id <- ""
  
  for (line in fasta_content) {
    if (grepl("^>", line)) {
      current_id <- gsub("^>", "", line)
      sequences[[current_id]] <- ""
    } else {
      if (length(sequences[[current_id]]) == 0) {
        sequences[[current_id]] <- line
      } else {
        sequences[[current_id]] <- paste0(sequences[[current_id]], line)
      }
    }
  }
  
  # Get all IDs and sequences
  ids <- names(sequences)
  seqs <- unlist(sequences)
  
  # Find overlaps using vectorized operations
  result <- c()
  
  for (i in 1:length(seqs)) {
    for (j in 1:length(seqs)) {
      if (i != j) {
        # Check if suffix of seqs[i] matches prefix of seqs[j]
        if (nchar(seqs[i]) >= k && nchar(seqs[j]) >= k) {
          suffix <- substr(seqs[i], nchar(seqs[i]) - k + 1, nchar(seqs[i]))
          prefix <- substr(seqs[j], 1, k)
          
          if (suffix == prefix) {
            result <- c(result, paste0(ids[i], " ", ids[j]))
          }
        }
      }
    }
  }
  
  return(result)
}

# Example usage with sample data
# Create sample FASTA data for testing
sample_fasta <- c(
  ">Rosalind_0498",
  "AAATAAA",
  ">Rosalind_2391",
  "AAATTTT",
  ">Rosalind_2323",
  "TTTTTTT"
)

# Write sample data to file
writeLines(sample_fasta, "sample.fasta")

# Solve the problem
result <- overlap_graphs_efficient("sample.fasta", k = 3)
print(result)

# Clean up
file.remove("sample.fasta")
```

## Explanation

The solution works as follows:

1. **FASTA Parsing**: Read the FASTA file and parse the sequences into a list where keys are sequence IDs and values are the DNA sequences.

2. **Overlap Detection**: For each pair of sequences (A, B), check if the suffix of A (last k characters) matches the prefix of B (first k characters).

3. **Graph Construction**: When an overlap is found, create a directed edge from A to B in the graph.

4. **Output**: Return all directed edges in the format "from to".

## Key Features

- Handles FASTA format parsing correctly
- Efficient overlap checking with k-length suffix/prefix matching
- Returns directed edges in the required format
- Works with any overlap length k (default is 3)
- Handles multiple sequences in the input

## Time Complexity
O(n² × k) where n is the number of sequences and k is the overlap length.

## Space Complexity
O(n × m) where n is the number of sequences and m is the average sequence length.

The solution correctly identifies all directed edges in the overlap graph where one sequence's suffix matches another sequence's prefix.

