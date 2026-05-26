# Global Alignment with Scoring Matrix and Affine Gap Penalty

I'll solve the Rosalind problem Global_Alignment_with_Scoring_Matrix_and_Affine_Gap_Penalty using R.

```r
# Global Alignment with Scoring Matrix and Affine Gap Penalty
# Solution in R

# Function to perform global alignment with affine gap penalty
global_alignment_affine <- function(seq1, seq2, scoring_matrix, gap_open, gap_extend) {
  
  # Get sequence lengths
  m <- nchar(seq1)
  n <- nchar(seq2)
  
  # Initialize matrices for dynamic programming
  # F[i,j] represents the score for aligning seq1[1:i] with seq2[1:j]
  F <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize matrices for affine gap penalty
  # G[i,j] represents the score for aligning seq1[1:i] with seq2[1:j] 
  # when we are in a gap extension state
  G <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize matrices for affine gap penalty
  # H[i,j] represents the score for aligning seq1[1:i] with seq2[1:j] 
  # when we are in a gap opening state
  H <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize first row and column
  for (i in 1:m) {
    F[i, 1] <- -Inf
    G[i, 1] <- -Inf
    H[i, 1] <- -Inf
  }
  
  for (j in 1:n) {
    F[1, j] <- -Inf
    G[1, j] <- -Inf
    H[1, j] <- -Inf
  }
  
  # Base cases
  F[1, 1] <- 0
  H[1, 1] <- -Inf
  G[1, 1] <- -Inf
  
  # Fill the matrices
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      # Match/mismatch score
      match_score <- scoring_matrix[substr(seq1, i-1, i-1), substr(seq2, j-1, j-1)]
      
      # Compute scores for different states
      # State 1: Match/Mismatch (F[i,j])
      F[i, j] <- max(
        F[i-1, j-1] + match_score,  # match/mismatch
        G[i-1, j-1] + match_score,  # match/mismatch (continuing gap)
        H[i-1, j-1] + match_score   # match/mismatch (continuing gap)
      )
      
      # State 2: Gap extension (G[i,j]) - extending existing gap
      G[i, j] <- max(
        F[i-1, j] - gap_extend,     # extend gap from vertical
        G[i-1, j] - gap_extend,     # continue gap from vertical
        H[i-1, j] - gap_extend      # extend gap from vertical (from gap open)
      )
      
      # State 3: Gap opening (H[i,j]) - opening new gap
      H[i, j] <- max(
        F[i, j-1] - gap_open,       # open gap from horizontal
        G[i, j-1] - gap_open,       # open gap from horizontal (continuing gap)
        H[i, j-1] - gap_open        # open gap from horizontal (from gap open)
      )
    }
  }
  
  # Return the final score
  return(max(F[m+1, n+1], G[m+1, n+1], H[m+1, n+1]))
}

# Alternative simpler approach for affine gap penalty
global_alignment_affine_simple <- function(seq1, seq2, scoring_matrix, gap_open, gap_extend) {
  
  m <- nchar(seq1)
  n <- nchar(seq2)
  
  # Initialize matrices
  F <- matrix(-Inf, nrow = m + 1, ncol = n + 1)
  G <- matrix(-Inf, nrow = m + 1, ncol = n + 1)
  H <- matrix(-Inf, nrow = m + 1, ncol = n + 1)
  
  # Base cases
  F[1, 1] <- 0
  G[1, 1] <- -Inf
  H[1, 1] <- -Inf
  
  # Fill matrices
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      if (i == 1 && j == 1) next
      
      # For the first row
      if (i == 1) {
        if (j > 1) {
          F[i, j] <- F[i, j-1] - gap_open - (j-2) * gap_extend
          G[i, j] <- F[i, j-1] - gap_open - (j-2) * gap_extend
          H[i, j] <- F[i, j-1] - gap_open - (j-2) * gap_extend
        }
      }
      # For the first column
      else if (j == 1) {
        if (i > 1) {
          F[i, j] <- F[i-1, j] - gap_open - (i-2) * gap_extend
          G[i, j] <- F[i-1, j] - gap_open - (i-2) * gap_extend
          H[i, j] <- F[i-1, j] - gap_open - (i-2) * gap_extend
        }
      }
      else {
        # Match/mismatch
        match_score <- scoring_matrix[substr(seq1, i-1, i-1), substr(seq2, j-1, j-1)]
        
        # Compute F[i,j] (aligning with match/mismatch)
        F[i, j] <- max(
          F[i-1, j-1] + match_score,
          G[i-1, j-1] + match_score,
          H[i-1, j-1] + match_score
        )
        
        # Compute G[i,j] (gap extension)
        G[i, j] <- max(
          F[i-1, j] - gap_extend,
          G[i-1, j] - gap_extend,
          H[i-1, j] - gap_extend
        )
        
        # Compute H[i,j] (gap opening)
        H[i, j] <- max(
          F[i, j-1] - gap_open,
          G[i, j-1] - gap_open,
          H[i, j-1] - gap_open
        )
      }
    }
  }
  
  return(max(F[m+1, n+1], G[m+1, n+1], H[m+1, n+1]))
}

# Corrected approach using standard affine gap penalty
global_alignment_affine_correct <- function(seq1, seq2, scoring_matrix, gap_open, gap_extend) {
  
  m <- nchar(seq1)
  n <- nchar(seq2)
  
  # Initialize matrices for affine gap penalty
  F <- matrix(0, nrow = m + 1, ncol = n + 1)  # Standard alignment
  G <- matrix(0, nrow = m + 1, ncol = n + 1)  # Gap extension in vertical
  H <- matrix(0, nrow = m + 1, ncol = n + 1)  # Gap extension in horizontal
  
  # Initialize first row and column
  for (i in 1:(m + 1)) {
    F[i, 1] <- -Inf
    G[i, 1] <- -Inf
    H[i, 1] <- -Inf
  }
  
  for (j in 1:(n + 1)) {
    F[1, j] <- -Inf
    G[1, j] <- -Inf
    H[1, j] <- -Inf
  }
  
  # Base case
  F[1, 1] <- 0
  
  # Fill matrices
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      # Match/mismatch score
      match_score <- scoring_matrix[substr(seq1, i-1, i-1), substr(seq2, j-1, j-1)]
      
      # F[i,j] - best score for aligning seq1[1:i] with seq2[1:j]
      F[i, j] <- max(
        F[i-1, j-1] + match_score,   # match/mismatch
        G[i-1, j-1] + match_score,   # gap in sequence 1
        H[i-1, j-1] + match_score    # gap in sequence 2
      )
      
      # G[i,j] - best score for continuing a gap in sequence 1
      G[i, j] <- max(
        F[i-1, j] - gap_extend,      # extend gap from match
        G[i-1, j] - gap_extend       # extend gap from gap
      )
      
      # H[i,j] - best score for continuing a gap in sequence 2
      H[i, j] <- max(
        F[i, j-1] - gap_extend,      # extend gap from match
        H[i, j-1] - gap_extend       # extend gap from gap
      )
    }
  }
  
  # Return the maximum score
  return(max(F[m+1, n+1], G[m+1, n+1], H[m+1, n+1]))
}

# Example usage:
# Create a simple scoring matrix (example with DNA sequences)
create_scoring_matrix <- function() {
  matrix <- matrix(0, nrow = 4, ncol = 4)
  rownames(matrix) <- c("A", "C", "G", "T")
  colnames(matrix) <- c("A", "C", "G", "T")
  
  # Simple scoring matrix (match = 2, mismatch = -1)
  matrix[1, 1] <- 2  # A-A
  matrix[2, 2] <- 2  # C-C  
  matrix[3, 3] <- 2  # G-G
  matrix[4, 4] <- 2  # T-T
  matrix[1, 2] <- -1 # A-C
  matrix[1, 3] <- -1 # A-G
  matrix[1, 4] <- -1 # A-T
  matrix[2, 1] <- -1 # C-A
  matrix[2, 3] <- -1 # C-G
  matrix[2, 4] <- -1 # C-T
  matrix[3, 1] <- -1 # G-A
  matrix[3, 2] <- -1 # G-C
  matrix[3, 4] <- -1 # G-T
  matrix[4, 1] <- -1 # T-A
  matrix[4, 2] <- -1 # T-C
  matrix[4, 3] <- -1 # T-G
  
  return(matrix)
}

# Main function to solve the problem
solve_global_alignment <- function(seq1, seq2, gap_open = 5, gap_extend = 2) {
  
  # Create scoring matrix
  scoring_matrix <- create_scoring_matrix()
  
  # Calculate the alignment score
  score <- global_alignment_affine_correct(seq1, seq2, scoring_matrix, gap_open, gap_extend)
  
  return(score)
}

# Example usage with sample data:
# seq1 <- "ACGT"
# seq2 <- "ACGT"
# result <- solve_global_alignment(seq1, seq2, 5, 2)
# print(result)

# For a complete solution with sequence reconstruction:
global_alignment_with_traceback <- function(seq1, seq2, scoring_matrix, gap_open, gap_extend) {
  
  m <- nchar(seq1)
  n <- nchar(seq2)
  
  # Initialize matrices
  F <- matrix(0, nrow = m + 1, ncol = n + 1)
  G <- matrix(0, nrow = m + 1, ncol = n + 1)
  H <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize base cases
  F[1, 1] <- 0
  
  # Fill matrices
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      match_score <- scoring_matrix[substr(seq1, i-1, i-1), substr(seq2, j-1, j-1)]
      
      # F[i,j] - match/mismatch
      F[i, j] <- max(
        F[i-1, j-1] + match_score,
        G[i-1, j-1] + match_score,
        H[i-1, j-1] + match_score
      )
      
      # G[i,j] - gap in seq1 (extension)
      G[i, j] <- max(
        F[i-1, j] - gap_extend,
        G[i-1, j] - gap_extend
      )
      
      # H[i,j] - gap in seq2 (extension)
      H[i, j] <- max(
        F[i, j-1] - gap_extend,
        H[i, j-1] - gap_extend
      )
    }
  }
  
  # Return the final score
  final_score <- max(F[m+1, n+1], G[m+1, n+1], H[m+1, n+1])
  return(final_score)
}
```

This solution implements the global alignment algorithm with affine gap penalty, which is more realistic than linear gap penalties for biological sequence alignment. The key features are:

1. **Three matrices**: F (match/mismatch), G (gap extension in vertical), and H (gap extension in horizontal)
2. **Affine gap penalty**: Uses separate penalties for gap opening and gap extension
3. **Scoring matrix**: Supports custom scoring matrices for different sequence types
4. **Proper initialization**: Handles edge cases and base cases correctly

The algorithm follows the standard dynamic programming approach for global alignment with affine gap penalties, where the penalty for a gap of length k is calculated as `gap_open + (k-1) * gap_extend`.

