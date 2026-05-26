# Rosalind Problem: Transcribing DNA into RNA

## Problem Statement
In RNA, the nucleotide uracil (U) is used instead of thymine (T). Given a DNA string, we need to transcribe it into RNA by replacing each occurrence of 'T' with 'U'.

## Solution in R

```r
# Function to transcribe DNA to RNA
dna_to_rna <- function(dna_string) {
  # Replace all 'T' with 'U'
  rna_string <- gsub("T", "U", dna_string)
  return(rna_string)
}

# Alternative approach using stringr package
# library(stringr)
# dna_to_rna <- function(dna_string) {
#   str_replace_all(dna_string, "T", "U")
# }

# Example usage
dna_sequence <- "GATGGAACTTGACTACGTAAATT"
rna_sequence <- dna_to_rna(dna_sequence)
print(rna_sequence)

# Expected output: "GAUGGAACUUGACUACGUAAAUU"
```

## Step-by-step explanation:

1. **Function definition**: Create a function `dna_to_rna` that takes a DNA string as input
2. **String replacement**: Use `gsub("T", "U", dna_string)` to replace all occurrences of 'T' with 'U'
3. **Return result**: Return the transcribed RNA string

## Alternative approaches:

```r
# Using character vector approach
dna_to_rna_vector <- function(dna_string) {
  # Convert to character vector, replace, then paste back
  dna_chars <- unlist(strsplit(dna_string, ""))
  rna_chars <- ifelse(dna_chars == "T", "U", dna_chars)
  paste(rna_chars, collapse = "")
}

# Using base R with gsub (more concise)
transcribe <- function(dna) gsub("T", "U", dna)
```

## Test with sample data:
```r
# Test case from Rosalind
test_dna <- "GATGGAACTTGACTACGTAAATT"
result <- dna_to_rna(test_dna)
print(result)  # Should output: GAUGGAACUUGACUACGUAAAUU
```

The solution uses R's built-in `gsub()` function which efficiently replaces all occurrences of the pattern 'T' with 'U' in the DNA string, producing the correct RNA transcription.

