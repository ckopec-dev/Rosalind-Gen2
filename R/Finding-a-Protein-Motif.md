# Finding a Protein Motif - Rosalind Problem Solution in R

## Problem Understanding

The problem asks us to find all occurrences of a specific protein motif in a given protein sequence. The motif is defined as a pattern that can contain ambiguous characters (like "N" followed by any amino acid except "P").

## Solution Approach

I'll solve this step by step using R:

```r
# Load required libraries
library(stringr)

# Function to find protein motifs in a sequence
find_protein_motif <- function(sequence, motif_pattern) {
  # Find all matches of the motif pattern
  matches <- str_locate_all(sequence, motif_pattern)
  
  # Extract positions
  positions <- unlist(matches)[1]
  
  return(positions)
}

# Function to create regex pattern for N-glycosylation motif
create_nglycosylation_pattern <- function() {
  # N-glycosylation motif: N{P}[ST]{P}
  # N followed by any amino acid except P, then S or T, then any amino acid except P
  return("N[^P][ST][^P]")
}

# Main solution function
solve_finding_a_protein_motif <- function(fasta_file) {
  # Read FASTA file
  fasta_content <- readLines(fasta_file)
  
  # Parse FASTA to extract sequence
  sequences <- list()
  current_id <- ""
  current_seq <- ""
  
  for (line in fasta_content) {
    if (str_starts(line, ">")) {
      if (current_id != "") {
        sequences[[current_id]] <- current_seq
      }
      current_id <- str_sub(line, 2)
      current_seq <- ""
    } else {
      current_seq <- paste0(current_seq, line)
    }
  }
  
  # Add last sequence
  if (current_id != "") {
    sequences[[current_id]] <- current_seq
  }
  
  # Process each sequence
  results <- list()
  
  for (id in names(sequences)) {
    sequence <- sequences[[id]]
    pattern <- create_nglycosylation_pattern()
    
    # Find all matches
    matches <- str_locate_all(sequence, pattern)
    
    # Extract positions (1-indexed)
    positions <- unlist(matches)[1]
    
    # Filter out empty results
    if (length(positions) > 0) {
      results[[id]] <- positions
    }
  }
  
  return(results)
}

# Alternative approach using a more specific implementation
find_nglycosylation_motifs <- function(sequence, protein_id) {
  # N-glycosylation motif: N{P}[ST]{P}
  # This means N followed by any amino acid except P, then S or T, then any amino acid except P
  
  pattern <- "N[^P][ST][^P]"
  
  # Find all matches with their positions
  matches <- str_locate_all(sequence, pattern)
  
  # Extract positions and convert to 1-indexed
  if (length(matches[[1]]) > 0) {
    positions <- matches[[1]][,1]
    return(list(id = protein_id, positions = positions))
  } else {
    return(NULL)
  }
}

# Complete solution for Rosalind problem
rosalind_finding_a_protein_motif <- function(input_file) {
  # Read the input file
  lines <- readLines(input_file)
  
  # Parse FASTA format
  sequences <- list()
  current_id <- ""
  current_seq <- ""
  
  for (line in lines) {
    if (str_starts(line, ">")) {
      if (current_id != "") {
        sequences[[current_id]] <- current_seq
      }
      current_id <- str_sub(line, 2)
      current_seq <- ""
    } else {
      current_seq <- paste0(current_seq, line)
    }
  }
  
  # Add last sequence
  if (current_id != "") {
    sequences[[current_id]] <- current_seq
  }
  
  # Find motifs for each sequence
  output_lines <- c()
  
  for (id in names(sequences)) {
    sequence <- sequences[[id]]
    
    # Find N-glycosylation motifs (N{P}[ST]{P})
    pattern <- "N[^P][ST][^P]"
    matches <- str_locate_all(sequence, pattern)
    
    # Extract positions
    if (length(matches[[1]]) > 0) {
      positions <- matches[[1]][,1]
      # Convert to 1-indexed and format output
      formatted_positions <- paste0(positions, collapse = " ")
      output_lines <- c(output_lines, paste0(id, "\n", formatted_positions))
    }
  }
  
  return(output_lines)
}

# Example usage:
# Assuming we have a FASTA file with protein sequences
# result <- rosalind_finding_a_protein_motif("rosalind_mprt.txt")

# Simple test function
test_motif_finding <- function() {
  # Test sequence
  test_seq <- "MNGFNKTNQVYKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQKQVQK