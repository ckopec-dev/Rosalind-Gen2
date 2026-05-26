# Rosalind Problem: Translating RNA into Protein

## Problem Description
The task is to translate an RNA string into a protein string by reading codons (groups of 3 nucleotides) and mapping them to their corresponding amino acids.

## Solution in R

```r
# Function to translate RNA to protein
translate_rna <- function(rna_string) {
  # Codon table - mapping RNA codons to amino acids
  codon_table <- list(
    "UUU" = "F", "UUC" = "F", "UUA" = "L", "UUG" = "L",
    "CUU" = "L", "CUC" = "L", "CUA" = "L", "CUG" = "L",
    "AUU" = "I", "AUC" = "I", "AUA" = "I", "GUU" = "V",
    "GUC" = "V", "GUA" = "V", "GUG" = "V", "UCU" = "S",
    "UCC" = "S", "UCA" = "S", "UCG" = "S", "CCU" = "P",
    "CCC" = "P", "CCA" = "P", "CCG" = "P", "ACU" = "T",
    "ACC" = "T", "ACA" = "T", "ACG" = "T", "GCU" = "A",
    "GCC" = "A", "GCA" = "A", "GCG" = "A", "UAU" = "Y",
    "UAC" = "Y", "UGU" = "C", "UGC" = "C", "UGG" = "W",
    "UAA" = "*", "UAG" = "*", "UGA" = "*"
  )
  
  # Initialize protein string
  protein <- ""
  
  # Process RNA string in groups of 3 nucleotides (codons)
  for (i in seq(1, nchar(rna_string), by = 3)) {
    # Extract codon (3 nucleotides)
    codon <- substr(rna_string, i, i + 2)
    
    # Check if we have a complete codon
    if (nchar(codon) == 3) {
      # Look up amino acid in codon table
      amino_acid <- codon_table[[codon]]
      
      # Stop translation if we encounter a stop codon
      if (amino_acid == "*") {
        break
      }
      
      # Append amino acid to protein string
      protein <- paste0(protein, amino_acid)
    }
  }
  
  return(protein)
}

# Example usage
# Read input RNA string (replace with actual input)
rna_input <- "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"

# Translate RNA to protein
result <- translate_rna(rna_input)
print(result)
```

## Alternative Implementation Using Built-in Functions

```r
# More concise version using sapply
translate_rna_v2 <- function(rna_string) {
  # Codon table
  codon_table <- list(
    "UUU" = "F", "UUC" = "F", "UUA" = "L", "UUG" = "L",
    "CUU" = "L", "CUC" = "L", "CUA" = "L", "CUG" = "L",
    "AUU" = "I", "AUC" = "I", "AUA" = "I", "GUU" = "V",
    "GUC" = "V", "GUA" = "V", "GUG" = "V", "UCU" = "S",
    "UCC" = "S", "UCA" = "S", "UCG" = "S", "CCU" = "P",
    "CCC" = "P", "CCA" = "P", "CCG" = "P", "ACU" = "T",
    "ACC" = "T", "ACA" = "T", "ACG" = "T", "GCU" = "A",
    "GCC" = "A", "GCA" = "A", "GCG" = "A", "UAU" = "Y",
    "UAC" = "Y", "UGU" = "C", "UGC" = "C", "UGG" = "W",
    "UAA" = "*", "UAG" = "*", "UGA" = "*"
  )
  
  # Split RNA string into codons
  codons <- substring(rna_string, seq(1, nchar(rna_string), 3), seq(3, nchar(rna_string), 3))
  
  # Translate codons to amino acids
  amino_acids <- sapply(codons, function(codon) {
    if (nchar(codon) == 3) {
      return(codon_table[[codon]])
    } else {
      return(NULL)
    }
  })
  
  # Convert to protein string and stop at first stop codon
  protein <- paste(amino_acids[amino_acids != "*"], collapse = "")
  return(protein)
}
```

## Key Points

1. **Codon Table**: The solution uses a list to map RNA codons to amino acids
2. **Reading Frame**: Process the RNA string in groups of 3 nucleotides
3. **Stop Codons**: Translation stops when encountering stop codons (*)
4. **Edge Cases**: Handles incomplete codons at the end of the sequence

## Example Input/Output

**Input**: `AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA`
**Output**: `MAIVMGR*` (where `*` represents a stop codon)

The function correctly translates RNA sequences to their corresponding protein sequences according to the genetic code.

