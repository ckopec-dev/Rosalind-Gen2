# Rosalind Problem: Assessing Assembly Quality with N50 and N75

## Problem Understanding

The N50 and N75 statistics are commonly used to assess the quality of genome assemblies. They represent the contig length at which 50% and 75% of the total assembly length is contained in contigs of that length or longer.

## Solution in R

```r
# Function to calculate N50 and N75 statistics
calculate_n50_n75 <- function(contig_lengths) {
  # Sort contig lengths in descending order
  sorted_lengths <- sort(contig_lengths, decreasing = TRUE)
  
  # Calculate total assembly length
  total_length <- sum(sorted_lengths)
  
  # Calculate cumulative sum
  cumulative_sum <- cumsum(sorted_lengths)
  
  # Find N50 (length where 50% of assembly is covered)
  n50_index <- which(cumulative_sum >= total_length * 0.5)[1]
  n50 <- sorted_lengths[n50_index]
  
  # Find N75 (length where 75% of assembly is covered)
  n75_index <- which(cumulative_sum >= total_length * 0.75)[1]
  n75 <- sorted_lengths[n75_index]
  
  return(list(N50 = n50, N75 = n75))
}

# Alternative implementation with more detailed output
calculate_assembly_stats <- function(contig_lengths) {
  # Sort contig lengths in descending order
  sorted_lengths <- sort(contig_lengths, decreasing = TRUE)
  
  # Calculate total assembly length
  total_length <- sum(sorted_lengths)
  
  # Calculate cumulative sum
  cumulative_sum <- cumsum(sorted_lengths)
  
  # Calculate N50
  n50_index <- which(cumulative_sum >= total_length * 0.5)[1]
  n50 <- sorted_lengths[n50_index]
  
  # Calculate N75
  n75_index <- which(cumulative_sum >= total_length * 0.75)[1]
  n75 <- sorted_lengths[n75_index]
  
  # Additional statistics
  num_contigs <- length(contig_lengths)
  largest_contig <- max(contig_lengths)
  smallest_contig <- min(contig_lengths)
  
  return(list(
    N50 = n50,
    N75 = n75,
    total_length = total_length,
    num_contigs = num_contigs,
    largest_contig = largest_contig,
    smallest_contig = smallest_contig
  ))
}

# Example usage with sample data
# Sample contig lengths (you would replace this with your actual data)
sample_contigs <- c(1000, 500, 2000, 1500, 800, 300, 1200, 600, 900, 400)

# Calculate statistics
stats <- calculate_assembly_stats(sample_contigs)

# Print results
cat("Assembly Quality Statistics:\n")
cat("N50:", stats$N50, "\n")
cat("N75:", stats$N75, "\n")
cat("Total Assembly Length:", stats$total_length, "\n")
cat("Number of Contigs:", stats$num_contigs, "\n")
cat("Largest Contig:", stats$largest_contig, "\n")
cat("Smallest Contig:", stats$smallest_contig, "\n")

# For direct Rosalind input processing
process_rosalind_input <- function(input_file = NULL, contig_vector = NULL) {
  if (!is.null(input_file)) {
    # Read from file
    contig_lengths <- as.numeric(readLines(input_file))
  } else if (!is.null(contig_vector)) {
    # Use provided vector
    contig_lengths <- contig_vector
  } else {
    stop("Either input_file or contig_vector must be provided")
  }
  
  # Calculate N50 and N75
  result <- calculate_n50_n75(contig_lengths)
  
  return(result)
}

# Example of how to use with Rosalind format input
# Assuming you have a file with one contig length per line
# result <- process_rosalind_input(input_file = "rosalind_input.txt")

# Simple example with specific values
example_lengths <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

# Calculate N50 and N75
result <- calculate_n50_n75(example_lengths)
cat("Example Results:\n")
cat("N50:", result$N50, "\n")
cat("N75:", result$N75, "\n")
```

## Key Points:

1. **N50 Calculation**: The contig length at which 50% of the total assembly is contained in contigs of that length or longer
2. **N75 Calculation**: The contig length at which 75% of the total assembly is contained in contigs of that length or longer
3. **Sorting**: Contig lengths must be sorted in descending order
4. **Cumulative Sum**: Used to determine where the 50% and 75% thresholds are reached

## How to Use:

1. Replace the sample contig lengths with your actual data
2. The function will return both N50 and N75 values
3. For Rosalind input, you can read from a file or pass a vector directly

The solution handles the core algorithm for calculating these important assembly quality metrics.

