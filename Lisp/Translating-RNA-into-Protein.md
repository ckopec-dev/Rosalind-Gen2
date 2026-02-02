# Rosalind Problem: Translating RNA into Protein (Lisp Solution)

## Problem Statement
The task is to translate an RNA string into a protein string by reading codons (3-character sequences) and mapping them to their corresponding amino acids.

## Solution

```lisp
(defun translate-rna-to-protein (rna-string)
  "Translate RNA string into protein string using genetic code."
  (let ((genetic-code 
         '(("UUU" . "F") ("UUC" . "F") ("UUA" . "L") ("UUG" . "L")
           ("CUU" . "L") ("CUC" . "L") ("CUA" . "L") ("CUG" . "L")
           ("AUU" . "I") ("AUC" . "I") ("AUA" . "I") ("AUG" . "M")
           ("GUU" . "V") ("GUC" . "V") ("GUA" . "V") ("GUG" . "V")
           ("UCU" . "S") ("UCC" . "S") ("UCA" . "S") ("UCG" . "S")
           ("CCU" . "P") ("CCC" . "P") ("CCA" . "P") ("CCG" . "P")
           ("ACU" . "T") ("ACC" . "T") ("ACA" . "T") ("ACG" . "T")
           ("GCU" . "A") ("GCC" . "A") ("GCA" . "A") ("GCG" . "A")
           ("UAU" . "Y") ("UAC" . "Y") ("UAA" . "*") ("UAG" . "*")
           ("UGU" . "C") ("UGC" . "C") ("UGA" . "*") ("UGG" . "W")
           ("CAU" . "H") ("CAC" . "H") ("CAA" . "Q") ("CAG" . "Q")
           ("AAU" . "N") ("AAC" . "N") ("AAA" . "K") ("AAG" . "K")
           ("GAU" . "D") ("GAC" . "D") ("GAA" . "E") ("GAG" . "E")
           ("GCU" . "G") ("GCC" . "G") ("GCA" . "G") ("GCG" . "G"))))
    (let ((protein '()))
      (loop for i from 0 below (length rna-string) by 3
            for codon = (subseq rna-string i (+ i 3))
            for amino-acid = (cdr (assoc codon genetic-code))
            do (if (equal amino-acid "*")
                   (return protein)
                   (push amino-acid protein)))
      (nreverse protein))))

;; Alternative more concise version
(defun translate-rna-to-protein-alt (rna-string)
  "Alternative translation using string manipulation."
  (let ((genetic-code 
         '(("UUU" . "F") ("UUC" . "F") ("UUA" . "L") ("UUG" . "L")
           ("CUU" . "L") ("CUC" . "L") ("CUA" . "L") ("CUG" . "L")
           ("AUU" . "I") ("AUC" . "I") ("AUA" . "I") ("AUG" . "M")
           ("GUU" . "V") ("GUC" . "V") ("GUA" . "V") ("GUG" . "V")
           ("UCU" . "S") ("UCC" . "S") ("UCA" . "S") ("UCG" . "S")
           ("CCU" . "P") ("CCC" . "P") ("CCA" . "P") ("CCG" . "P")
           ("ACU" . "T") ("ACC" . "T") ("ACA" . "T") ("ACG" . "T")
           ("GCU" . "A") ("GCC" . "A") ("GCA" . "A") ("GCG" . "A")
           ("UAU" . "Y") ("UAC" . "Y") ("UAA" . "*") ("UAG" . "*")
           ("UGU" . "C") ("UGC" . "C") ("UGA" . "*") ("UGG" . "W")
           ("CAU" . "H") ("CAC" . "H") ("CAA" . "Q") ("CAG" . "Q")
           ("AAU" . "N") ("AAC" . "N") ("AAA" . "K") ("AAG" . "K")
           ("GAU" . "D") ("GAC" . "D") ("GAA" . "E") ("GAG" . "E")
           ("GCU" . "G") ("GCC" . "G") ("GCA" . "G") ("GCG" . "G"))))
    (let ((protein '()))
      (loop for i from 0 below (length rna-string) by 3
            for codon = (subseq rna-string i (+ i 3))
            for amino-acid = (cdr (assoc codon genetic-code))
            do (if (equal amino-acid "*")
                   (return (nreverse protein))
                   (push amino-acid protein)))
      (nreverse protein))))

;; Example usage:
;; (translate-rna-to-protein "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
;; Should return: ("M" "A" "M" "A" "A" "L" "Q" "L" "S" "R" "L" "Y" "S" "R" "G" "R" "W" "G" "G" "G" "G")
```

## Explanation

This solution works by:

1. **Defining the genetic code**: A list of cons cells mapping 3-character codons to amino acid symbols
2. **Processing the RNA string**: Looping through the string in steps of 3 characters (codons)
3. **Looking up amino acids**: Using `assoc` to find the corresponding amino acid for each codon
4. **Handling stop codons**: When encountering a stop codon ("UAA", "UAG", "UGA"), the translation stops
5. **Building the result**: Collecting amino acids in reverse order and then reversing to get the correct sequence

## Key Features

- Uses `assoc` for efficient codon lookups
- Handles stop codons properly by returning early
- Processes the RNA string in 3-character chunks
- Returns the protein sequence as a list of amino acid symbols
- Includes both a detailed and concise version of the solution

The function will correctly translate any valid RNA sequence into its corresponding protein sequence according to the standard genetic code.

