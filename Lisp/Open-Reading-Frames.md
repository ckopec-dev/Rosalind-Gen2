# Rosalind Problem: Open Reading Frames (ORF)

## Problem Description
Given a DNA string, find all possible open reading frames (ORFs) and translate them to proteins.

## Solution in Lisp

```lisp
(defun dna-to-rna (dna)
  "Convert DNA string to RNA string"
  (substitute #\U #\T dna))

(defun rna-to-protein (rna)
  "Translate RNA string to protein string"
  (let ((codon-table '(("UUU" . "F") ("UUC" . "F") ("UUA" . "L") ("UUG" . "L")
                       ("CUU" . "L") ("CUC" . "L") ("CUA" . "L") ("CUG" . "L")
                       ("AUU" . "I") ("AUC" . "I") ("AUA" . "I") ("GUU" . "V")
                       ("GUC" . "V") ("GUA" . "V") ("GUG" . "V") ("UCU" . "S")
                       ("UCC" . "S") ("UCA" . "S") ("UCG" . "S") ("CCU" . "P")
                       ("CCC" . "P") ("CCA" . "P") ("CCG" . "P") ("ACU" . "T")
                       ("ACC" . "T") ("ACA" . "T") ("ACG" . "T") ("GCU" . "A")
                       ("GCC" . "A") ("GCA" . "A") ("GCG" . "A") ("UAU" . "Y")
                       ("UAC" . "Y") ("UGU" . "C") ("UGC" . "C") ("UGG" . "W")
                       ("UAA" . "") ("UAG" . "") ("UGA" . ""))))
    (let ((proteins '()))
      (loop for i from 0 below (length rna) by 3
            do (let ((codon (subseq rna i (min (+ i 3) (length rna)))))
                 (when (= (length codon) 3)
                   (let ((protein (cdr (assoc codon codon-table))))
                     (if (string= protein "")
                         (return-from rna-to-protein (reverse proteins))
                         (push protein proteins))))))
      (reverse proteins))))

(defun find-orfs (dna)
  "Find all open reading frames in DNA string"
  (let ((orfs '()))
    ;; Forward strand
    (loop for i from 0 below (length dna) do
          (when (string= (subseq dna i (+ i 3)) "ATG")
            (let ((start i))
              (loop for j from (+ i 3) below (length dna) by 3
                    do (let ((codon (subseq dna j (min (+ j 3) (length dna)))))
                         (when (member codon '("TAA" "TAG" "TGA"))
                           (push (subseq dna start j) orfs)
                           (return))
                         (when (> (+ j 3) (length dna))
                           (return)))))))
    
    ;; Reverse complement strand
    (let ((rev-dna (reverse-complement dna)))
      (loop for i from 0 below (length rev-dna) do
            (when (string= (subseq rev-dna i (+ i 3)) "ATG")
              (let ((start i))
                (loop for j from (+ i 3) below (length rev-dna) by 3
                      do (let ((codon (subseq rev-dna j (min (+ j 3) (length rev-dna)))))
                           (when (member codon '("TAA" "TAG" "TGA"))
                             (push (subseq rev-dna start j) orfs)
                             (return))
                           (when (> (+ j 3) (length rev-dna))
                             (return))))))))
    orfs))

(defun reverse-complement (dna)
  "Get reverse complement of DNA string"
  (let ((complement-table '(("A" . "T") ("T" . "A") ("G" . "C") ("C" . "G"))))
    (let ((result '()))
      (loop for i from (1- (length dna)) downto 0
            do (let ((base (string (char dna i))))
                 (push (cdr (assoc base complement-table)) result))))
      (apply #'concatenate 'string result))))

(defun solve-orf (dna)
  "Solve the ORF problem"
  (let ((orfs (find-orfs dna))
        (proteins '()))
    (loop for orf in orfs
          do (let ((rna (dna-to-rna orf)))
               (let ((protein (rna-to-protein rna)))
                 (when (and protein (not (string= protein "")))
                   (push protein proteins)))))
    (remove-duplicates proteins)))

;; Example usage:
;; (solve-orf "AGCCATGTAGCTAAGCGAATGTAGCTA")
```

## Explanation

This solution implements the Open Reading Frames problem by:

1. **DNA to RNA conversion**: Replacing T with U in the DNA string
2. **RNA to protein translation**: Using a codon table to translate RNA codons to amino acids
3. **ORF finding**: 
   - Scanning both forward and reverse complement strands
   - Looking for start codon "ATG" and stop codons "TAA", "TAG", "TGA"
4. **Reverse complement**: Computing the reverse complement of the DNA strand
5. **Protein extraction**: Converting valid ORFs to proteins and removing duplicates

## Key Functions

- `dna-to-rna`: Converts DNA to RNA by replacing T with U
- `rna-to-protein`: Translates RNA codons to proteins using the genetic code
- `find-orfs`: Identifies all open reading frames in both strands
- `reverse-complement`: Computes the reverse complement of DNA
- `solve-orf`: Main function that returns unique proteins from all ORFs

The solution handles both forward and reverse strands and returns all unique proteins that can be translated from valid ORFs.

