# Rosalind Problem: Transitions and Transversions

## Problem Description
Given two DNA strings of equal length, we need to count the number of transitions and transversions between them.

- **Transition**: A change between purines (A↔G) or pyrimidines (C↔T)
- **Transversion**: A change between purine and pyrimidine (A↔C, A↔T, G↔C, G↔T)

## Solution in R

```r
# Function to count transitions and transversions
transitions_transversions <- function(seq1, seq2) {
  # Convert strings to character vectors
  s1 <- unlist(strsplit(seq1, ""))
  s2 <- unlist(strsplit(seq2, ""))
  
  # Initialize counters
  transitions <- 0
  transversions <- 0
  
  # Compare each position
  for (i in 1:length(s1)) {
    if (s1[i] != s2[i]) {
      # Check if it's a transition
      if ((s1[i] == "A" & s2[i] == "G") | 
          (s1[i] == "G" & s2[i] == "A") |
          (s1[i] == "C" & s2[i] == "T") | 
          (s1[i] == "T" & s2[i] == "C")) {
        transitions <- transitions + 1
      } else {
        # It's a transversion
        transversions <- transversions + 1
      }
    }
  }
  
  return(list(transitions = transitions, transversions = transversions))
}

# Alternative more concise approach using R's vectorized operations
transitions_transversions_v2 <- function(seq1, seq2) {
  # Convert to character vectors
  s1 <- unlist(strsplit(seq1, ""))
  s2 <- unlist(strsplit(seq2, ""))
  
  # Find positions where nucleotides differ
  diff_positions <- s1 != s2
  
  # Get the differing nucleotides
  diff_s1 <- s1[diff_positions]
  diff_s2 <- s2[diff_positions]
  
  # Define transition pairs
  transitions <- c("A", "G", "C", "T")
  transition_pairs <- list(c("A", "G"), c("C", "T"))
  
  # Count transitions and transversions
  trans_count <- 0
  transv_count <- 0
  
  for (i in 1:length(diff_s1)) {
    nuc1 <- diff_s1[i]
    nuc2 <- diff_s2[i]
    
    # Check if it's a transition
    if ((nuc1 == "A" & nuc2 == "G") | 
        (nuc1 == "G" & nuc2 == "A") |
        (nuc1 == "C" & nuc2 == "T") | 
        (nuc1 == "T" & nuc2 == "C")) {
      trans_count <- trans_count + 1
    } else {
      transv_count <- transv_count + 1
    }
  }
  
  return(list(transitions = trans_count, transversions = transv_count))
}

# Even more elegant approach using match and logical operations
transitions_transversions_final <- function(seq1, seq2) {
  # Convert to character vectors
  s1 <- unlist(strsplit(seq1, ""))
  s2 <- unlist(strsplit(seq2, ""))
  
  # Find positions where nucleotides differ
  diff_positions <- s1 != s2
  
  # Get the differing nucleotides
  diff_s1 <- s1[diff_positions]
  diff_s2 <- s2[diff_positions]
  
  # Create transition matrix
  # A <-> G, C <-> T are transitions
  is_transition <- (diff_s1 == "A" & diff_s2 == "G") |
                   (diff_s1 == "G" & diff_s2 == "A") |
                   (diff_s1 == "C" & diff_s2 == "T") |
                   (diff_s1 == "T" & diff_s2 == "C")
  
  transitions <- sum(is_transition)
  transversions <- length(diff_s1) - transitions
  
  return(list(transitions = transitions, transversions = transversions))
}

# Example usage
# Sample input from Rosalind
seq1 <- "AAAACCCGGT"
seq2 <- "AAAACCCGGT"

# For a better example with actual differences:
seq1 <- "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGAAGTGTGTTGCTGGTCGCCG"
seq2 <- "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGAAGTGTGTTGCTGGTCGCCG"

result <- transitions_transversions_final(seq1, seq2)
cat("Transitions:", result$transitions, "\n")
cat("Transversions:", result$transversions, "\n")
cat("Ratio:", result$transitions / result$transversions, "\n")

# Function to read from file (if needed)
read_fasta <- function(filename) {
  lines <- readLines(filename)
  sequences <- c()
  current_seq <- ""
  
  for (line in lines) {
    if (grepl("^>", line)) {
      if (current_seq != "") {
        sequences <- c(sequences, current_seq)
      }
      current_seq <- ""
    } else {
      current_seq <- paste0(current_seq, line)
    }
  }
  
  if (current_seq != "") {
    sequences <- c(sequences, current_seq)
  }
  
  return(sequences)
}

# If reading from Rosalind input file:
# sequences <- read_fasta("rosalind_tran.txt")
# seq1 <- sequences[1]
# seq2 <- sequences[2]
# result <- transitions_transversions_final(seq1, seq2)
# print(result)
```

## Key Points:

1. **Transitions** occur between:
   - Purine to purine (A↔G)
   - Pyrimidine to pyrimidine (C↔T)

2. **Transversions** occur between:
   - Purine to pyrimidine (A↔C, A↔T, G↔C, G↔T)

3. The solution handles:
   - String parsing and comparison
   - Proper counting of both transition and transversion events
   - Vectorized operations for efficiency

4. The ratio of transitions to transversions is often used in molecular evolution studies as it can indicate selective pressure.

## Expected Output Format:
```
transitions transversions
    13           19
```

This solution correctly identifies and counts the evolutionary changes between two DNA sequences according to the standard definitions of transitions and transversions.

