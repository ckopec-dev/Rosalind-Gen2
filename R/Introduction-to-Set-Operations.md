# Rosalind Problem: Introduction to Set Operations (R Solution)

## Problem Description
Given two positive integers n and m, and two sets A and B of size at most n and m respectively, we need to compute:
- A ∪ B (union)
- A ∩ B (intersection) 
- A - B (difference)
- B - A (difference)
- A^c (complement of A in U)
- B^c (complement of B in U)

Where U is the universal set {1, 2, ..., n}.

## Solution in R

```r
# Read input data
# Assuming input is provided in a file called "rosalind_seto.txt"
# Format: first line contains n and m, second line contains set A, third line contains set B

# Read the input file
input <- readLines("rosalind_seto.txt")

# Parse first line to get n and m
nm <- as.numeric(unlist(strsplit(input[1], " ")))
n <- nm[1]
m <- nm[2]

# Parse sets A and B
A <- as.numeric(unlist(strsplit(input[2], " ")))
B <- as.numeric(unlist(strsplit(input[3], " ")))

# Create universal set U
U <- 1:n

# Compute set operations
union_AB <- union(A, B)
intersection_AB <- intersect(A, B)
diff_AB <- setdiff(A, B)
diff_BA <- setdiff(B, A)
complement_A <- setdiff(U, A)
complement_B <- setdiff(U, B)

# Print results
cat("A ∪ B:", paste(union_AB, collapse = " "), "\n")
cat("A ∩ B:", paste(intersection_AB, collapse = " "), "\n")
cat("A - B:", paste(diff_AB, collapse = " "), "\n")
cat("B - A:", paste(diff_BA, collapse = " "), "\n")
cat("A^c:", paste(complement_A, collapse = " "), "\n")
cat("B^c:", paste(complement_B, collapse = " "), "\n")
```

## Alternative Solution (More Compact)

```r
# Read input
input <- readLines("rosalind_seto.txt")
n <- as.numeric(unlist(strsplit(input[1], " ")))[1]
A <- as.numeric(unlist(strsplit(input[2], " ")))
B <- as.numeric(unlist(strsplit(input[3], " ")))

# Create universal set
U <- 1:n

# Perform all operations
results <- list(
  union = union(A, B),
  intersection = intersect(A, B),
  diff_A_B = setdiff(A, B),
  diff_B_A = setdiff(B, A),
  complement_A = setdiff(U, A),
  complement_B = setdiff(U, B)
)

# Print results in required format
cat("Union (A ∪ B):", paste(results$union, collapse = " "), "\n")
cat("Intersection (A ∩ B):", paste(results$intersection, collapse = " "), "\n")
cat("Difference (A - B):", paste(results$diff_A_B, collapse = " "), "\n")
cat("Difference (B - A):", paste(results$diff_B_A, collapse = " "), "\n")
cat("Complement (A^c):", paste(results$complement_A, collapse = " "), "\n")
cat("Complement (B^c):", paste(results$complement_B, collapse = " "), "\n")
```

## Example Input/Output

**Input:**
```
10 3
1 2 3 4 5
2 4 6
```

**Output:**
```
A ∪ B: 1 2 3 4 5 6
A ∩ B: 2 4
A - B: 1 3 5
B - A: 6
A^c: 6 7 8 9 10
B^c: 1 3 5 7 8 9 10
```

## Key R Functions Used

- `union(A, B)`: Computes the union of sets A and B
- `intersect(A, B)`: Computes the intersection of sets A and B  
- `setdiff(A, B)`: Computes the difference A - B
- `as.numeric()`: Converts character strings to numeric values
- `strsplit()`: Splits strings into character vectors
- `readLines()`: Reads text lines from a file

This solution handles the set operations efficiently using R's built-in set functions and properly formats the output as required by the Rosalind problem.

