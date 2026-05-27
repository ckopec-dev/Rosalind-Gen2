# Rosalind Problem RNA_Splicing Solution in Lisp

## Problem Understanding

RNA splicing involves removing introns (non-coding sequences) from pre-mRNA to form mature mRNA. Given a DNA string and a collection of introns, we need to:
1. Transcribe the DNA to RNA
2. Remove all introns from the RNA sequence
3. Translate the resulting mRNA to protein

## Solution Approach

```lisp
(defun transcribe (dna)
  "Convert DNA string to RNA string"
  (substitute #\U (find #\T dna) dna))

(defun remove-introns (rna introns)
  "Remove all introns from RNA string"
  (let ((result rna))
    (dolist (intron introns)
      (setf result (remove-substring result intron)))
    result))

(defun remove-substring (string substring)
  "Remove all occurrences of substring from string"
  (let ((pos (search substring string)))
    (if pos
        (concatenate 'string
                    (subseq string 0 pos)
                    (remove-substring (subseq string (+ pos (length substring))) substring))
        string)))

(defun rna-splicing (dna introns)
  "Main function to solve RNA splicing problem"
  (let* ((rna (transcribe dna))
         (spliced-rna (remove-introns rna introns)))
    spliced-rna))

;; Helper function to translate RNA to protein
(defun translate-rna (rna)
  "Translate RNA string to protein string"
  (let ((codon-table '(("UUU" . "F") ("UUC" . "F") ("UUA" . "L") ("UUG" . "L")
                       ("CUU" . "L") ("CUC" . "L") ("CUA" . "L") ("CUG" . "L")
                       ("AUU" . "I") ("AUC" . "I") ("AUA" . "I") ("UGU" . "C")
                       ("UGC" . "C") ("CAA" . "Q") ("CAG" . "Q") ("AAU" . "N")
                       ("AAC" . "N") ("GAU" . "D") ("GAC" . "D") ("AAA" . "K")
                       ("AAG" . "K") ("GAA" . "E") ("GAG" . "E") ("UGG" . "W")
                       ("CAU" . "H") ("CAC" . "H") ("CGU" . "R") ("CGC" . "R")
                       ("CGA" . "R") ("CGG" . "R") ("AGA" . "R") ("AGG" . "R")
                       ("AGU" . "S") ("AGC" . "S") ("UCU" . "S") ("UCC" . "S")
                       ("UCA" . "S") ("UCG" . "S") ("CCU" . "P") ("CCC" . "P")
                       ("CCA" . "P") ("CCG" . "P") ("ACU" . "T") ("ACC" . "T")
                       ("ACA" . "T") ("ACG" . "T") ("GCU" . "A") ("GCC" . "A")
                       ("GCA" . "A") ("GCG" . "A") ("GCU" . "G") ("GCC" . "G")
                       ("GCA" . "G") ("GCG" . "G") ("UAA" . "") ("UAG" . "")
                       ("UGA" . ""))))
    (let ((protein ""))
      (loop for i from 0 below (length rna) by 3
            while (<= (+ i 2) (length rna))
            do (let ((codon (subseq rna i (+ i 3))))
                 (let ((amino-acid (cdr (assoc codon codon-table))))
                   (if (string= amino-acid "")
                       (return protein)
                       (setf protein (concatenate 'string protein amino-acid))))))
      protein)))

;; Complete solution function
(defun solve-rna-splicing (dna introns)
  "Complete RNA splicing solution"
  (let ((spliced-rna (rna-splicing dna introns)))
    (translate-rna spliced-rna)))

;; Example usage:
;; (solve-rna-splicing "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG" 
;;                     '("ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"))
```

## Key Functions Explanation

1. **`transcribe`**: Converts DNA to RNA by replacing 'T' with 'U'
2. **`remove-introns`**: Removes all intron sequences from the RNA
3. **`remove-substring`**: Helper function to remove all occurrences of a substring
4. **`translate-rna`**: Converts RNA codons to amino acid sequence using the genetic code
5. **`solve-rna-splicing`**: Main function that combines all steps

## Time Complexity
- Transcription: O(n) where n is the length of DNA
- Intron removal: O(m × k) where m is the number of introns and k is the average length
- Translation: O(n/3) where n is the length of spliced RNA

## Space Complexity
O(n + m) where n is the length of the final sequence and m is the storage for introns

This solution handles the core RNA splicing problem by properly transcribing DNA to RNA, removing introns, and translating to the final protein sequence.

