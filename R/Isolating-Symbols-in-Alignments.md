# Rosalind Problem: Isolating Symbols in Alignments (R Solution)

## Problem Understanding

The problem asks us to find the number of symbols that appear in at least one of the sequences in a multiple sequence alignment, and count how many times each symbol appears across all sequences.

## Solution Approach

1. Parse the FASTA formatted input
2. Extract all sequences
3. Count occurrences of each symbol across all sequences
4. Return the count of unique symbols and their frequencies

## R Implementation

```r
# Function to solve Isolating Symbols in Alignments
isolate_symbols <- function(fasta_file) {
  # Read the FASTA file
  fasta_content <- readLines(fasta_file)
  
  # Parse FASTA sequences
  sequences <- list()
  current_seq <- ""
  current_id <- ""
  
  for (line in fasta_content) {
    if (startsWith(line, ">")) {
      if (current_id != "") {
        sequences[[current_id]] <- current_seq
      }
      current_id <- substr(line, 2, nchar(line))
      current_seq <- ""
    } else {
      current_seq <- paste0(current_seq, line)
    }
  }
  
  # Add the last sequence
  if (current_id != "") {
    sequences[[current_id]] <- current_seq
  }
  
  # Combine all sequences into one string
  all_chars <- paste(unlist(sequences), collapse = "")
  
  # Count frequency of each character
  char_freq <- table(strsplit(all_chars, "")[[1]])
  
  # Sort by frequency (descending)
  char_freq <- sort(char_freq, decreasing = TRUE)
  
  return(char_freq)
}

# Alternative approach using Biostrings package (more robust)
isolate_symbols_biostrings <- function(fasta_file) {
  # Load required library
  if (!require(Biostrings)) {
    install.packages("Biostrings")
    library(Biostrings)
  }
  
  # Read FASTA file
  seqs <- readDNAStringSet(fasta_file)
  
  # Convert to character vector and count symbols
  all_chars <- paste(as.character(seqs), collapse = "")
  char_freq <- table(strsplit(all_chars, "")[[1]])
  
  # Sort by frequency
  char_freq <- sort(char_freq, decreasing = TRUE)
  
  return(char_freq)
}

# Simple approach for given input format
isolate_symbols_simple <- function(input_text) {
  # Split input by lines
  lines <- unlist(strsplit(input_text, "\n"))
  
  # Extract sequences (assuming FASTA format)
  sequences <- list()
  current_seq <- ""
  current_id <- ""
  
  for (line in lines) {
    if (startsWith(line, ">")) {
      if (current_id != "") {
        sequences[[current_id]] <- current_seq
      }
      current_id <- substr(line, 2, nchar(line))
      current_seq <- ""
    } else {
      current_seq <- paste0(current_seq, line)
    }
  }
  
  # Add last sequence
  if (current_id != "") {
    sequences[[current_id]] <- current_seq
  }
  
  # Combine all sequences
  all_chars <- paste(unlist(sequences), collapse = "")
  
  # Count characters
  char_freq <- table(strsplit(all_chars, "")[[1]])
  
  # Sort by frequency
  char_freq <- sort(char_freq, decreasing = TRUE)
  
  return(char_freq)
}

# Example usage with sample data
sample_input <- ">Rosalind_1
GATTACA
>Rosalind_2
GACAACT
>Rosalind_3
GATATCA"

# Run the solution
result <- isolate_symbols_simple(sample_input)

# Print results
print("Symbol frequencies:")
print(result)

# Print just the counts
print("Number of unique symbols:")
print(length(result))

print("Total symbol occurrences:")
print(sum(result))
```

## Expected Output Format

For the example input:
```
>Rosalind_1
GATTACA
>Rosalind_2
GACAACT
>Rosalind_3
GATATCA
```

The output would show the frequency of each symbol:
- G: 3 occurrences
- A: 6 occurrences  
- T: 4 occurrences
- C: 3 occurrences
- T: 4 occurrences (already counted)
- A: 6 occurrences (already counted)

## Key Points

1. **Input parsing**: Handle FASTA format with > headers
2. **Character counting**: Use `table()` function for efficient counting
3. **Sorting**: Sort results by frequency in descending order
4. **Edge cases**: Handle empty sequences, different sequence lengths

## Usage

```r
# For a file input
result <- isolate_symbols("alignment.fasta")

# For direct text input
result <- isolate_symbols_simple(your_fasta_text)
```

This solution efficiently counts symbol frequencies across multiple aligned sequences and provides the required output format for the Rosalind problem.

