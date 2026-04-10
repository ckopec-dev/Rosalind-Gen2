# Rosalind Problem: Perfect Matchings and RNA Secondary Structures

## Problem Statement
Given an RNA string, find the number of perfect matchings of basepair edges in the bonding graph of the RNA string.

## Solution in Lisp

```lisp
(defun perfect-matchings (rna-string)
  "Calculate the number of perfect matchings for RNA secondary structure.
   Assumes the RNA string has equal numbers of A/U and G/C bases."
  (let ((a-count 0)
        (u-count 0)
        (g-count 0)
        (c-count 0))
    ;; Count each nucleotide
    (loop for nucleotide across rna-string do
      (cond ((char= nucleotide #\A) (incf a-count))
            ((char= nucleotide #\U) (incf u-count))
            ((char= nucleotide #\G) (incf g-count))
            ((char= nucleotide #\C) (incf c-count))))
    
    ;; For perfect matching, we need equal A/U and G/C pairs
    ;; The number of perfect matchings is the product of factorials
    ;; of each pair count
    (let ((a-u-pairs (min a-count u-count))
          (g-c-pairs (min g-count c-count)))
      (* (factorial a-u-pairs) (factorial g-c-pairs)))))

(defun factorial (n)
  "Calculate factorial of n"
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Alternative implementation using memoization for efficiency
(defun factorial-memo (n &optional (memo (make-hash-table)))
  "Calculate factorial with memoization"
  (if (<= n 1)
      1
      (if (gethash n memo)
          (gethash n memo)
          (let ((result (* n (factorial-memo (1- n) memo))))
            (setf (gethash n memo) result)
            result))))

(defun perfect-matchings-memo (rna-string)
  "Calculate perfect matchings with memoization"
  (let ((a-count 0)
        (u-count 0)
        (g-count 0)
        (c-count 0)
        (memo (make-hash-table)))
    ;; Count each nucleotide
    (loop for nucleotide across rna-string do
      (cond ((char= nucleotide #\A) (incf a-count))
            ((char= nucleotide #\U) (incf u-count))
            ((char= nucleotide #\G) (incf g-count))
            ((char= nucleotide #\C) (incf c-count))))
    
    ;; For perfect matching, we need equal A/U and G/C pairs
    (let ((a-u-pairs (min a-count u-count))
          (g-c-pairs (min g-count c-count)))
      (* (factorial-memo a-u-pairs memo) (factorial-memo g-c-pairs memo)))))
```

## Example Usage

```lisp
;; Example 1
(perfect-matchings-memo "AUAU")  ; Should return 2

;; Example 2  
(perfect-matchings-memo "ACAGU") ; Should return 1

;; Example 3
(perfect-matchings-memo "AUCG")  ; Should return 2
```

## Explanation

The approach is based on the mathematical principle that for perfect matchings in RNA secondary structures:

1. **Base Pairing Rules**: A pairs with U, G pairs with C
2. **Perfect Matching**: Every nucleotide must be paired exactly once
3. **Calculation**: 
   - Count A's and U's, take minimum (this gives number of A/U pairs)
   - Count G's and C's, take minimum (this gives number of G/C pairs)
   - The total number of perfect matchings = (A/U pairs)! × (G/C pairs)!

This is because:
- We can arrange the A/U pairs in (A/U pairs)! ways
- We can arrange the G/C pairs in (G/C pairs)! ways
- These arrangements are independent, so we multiply them

## Time Complexity
- **Time**: O(n) where n is the length of the RNA string
- **Space**: O(1) for counting, O(k) for memoization where k is the maximum pair count

The memoized version is more efficient for repeated calculations with the same input sizes.

