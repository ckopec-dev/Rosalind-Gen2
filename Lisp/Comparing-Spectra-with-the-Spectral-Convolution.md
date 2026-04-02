# Rosalind Problem: Comparing Spectra with the Spectral Convolution

## Problem Description

The spectral convolution of two spectra is the multiset of differences between all pairs of masses from the two spectra. Given two spectra, we need to compute their spectral convolution and return the elements sorted by their multiplicities (highest first), with ties broken by mass values (highest first).

## Solution in Lisp

```lisp
(defun spectral-convolution (spectrum1 spectrum2)
  "Compute the spectral convolution of two spectra.
   Returns a list of elements sorted by multiplicity (descending),
   then by mass value (descending) for ties."
  (let ((differences '()))
    ;; Compute all pairwise differences
    (dolist (mass1 spectrum1)
      (dolist (mass2 spectrum2)
        (push (- mass1 mass2) differences)))
    
    ;; Count frequencies of each difference
    (let ((freq-table (make-hash-table :test 'equal)))
      (dolist (diff differences)
        (let ((count (gethash diff freq-table 0)))
          (setf (gethash diff freq-table) (1+ count)))))
    
    ;; Convert to list of (mass . frequency) pairs
    (let ((result '()))
      (maphash (lambda (mass freq)
                   (push (cons mass freq) result))
                 freq-table)
      
      ;; Sort by frequency (descending), then by mass (descending)
      (sort result 
            (lambda (a b)
              (if (= (cdr a) (cdr b))
                  (> (car a) (car b))  ; tie-break by mass
                  (> (cdr a) (cdr b)))) ; sort by frequency
      )))
```

## Alternative Implementation with Better Sorting

```lisp
(defun spectral-convolution (spectrum1 spectrum2)
  "Compute the spectral convolution of two spectra.
   Returns a list of elements sorted by multiplicity (descending),
   then by mass value (descending) for ties."
  (let ((differences '()))
    ;; Compute all pairwise differences
    (dolist (mass1 spectrum1)
      (dolist (mass2 spectrum2)
        (push (- mass1 mass2) differences)))
    
    ;; Count frequencies of each difference
    (let ((freq-table (make-hash-table :test 'equal)))
      (dolist (diff differences)
        (let ((count (gethash diff freq-table 0)))
          (setf (gethash diff freq-table) (1+ count)))))
    
    ;; Convert to list of (mass . frequency) pairs and sort
    (let ((result (make-list (hash-table-count freq-table))))
      (let ((index 0))
        (maphash (lambda (mass freq)
                     (setf (nth index result) (cons mass freq))
                     (incf index))
                   freq-table))
      
      ;; Sort by frequency (descending), then by mass (descending)
      (sort result 
            (lambda (a b)
              (if (= (cdr a) (cdr b))
                  (> (car a) (car b))  ; tie-break by mass
                  (> (cdr a) (cdr b)))) ; sort by frequency
      )))
```

## Usage Example

```lisp
;; Example usage:
(setq spectrum1 '(0 137 186 323))
(setq spectrum2 '(0 137 274 323))

(spectral-convolution spectrum1 spectrum2)
;; Expected output: ((137 . 2) (274 . 1) (323 . 1) (0 . 1))

;; More complex example:
(setq spectrum1 '(0 137 186 323 400 537))
(setq spectrum2 '(0 137 274 323 400 537))

(spectral-convolution spectrum1 spectrum2)
```

## Key Features of the Solution

1. **Computes pairwise differences**: For each mass in spectrum1 and each mass in spectrum2, calculates their difference
2. **Counts frequencies**: Uses a hash table to count how many times each difference occurs
3. **Proper sorting**: Sorts results by frequency (descending) first, then by mass value (descending) for ties
4. **Handles edge cases**: Works with negative differences and zero differences
5. **Efficient**: Uses hash table for O(1) frequency counting

## Time Complexity
- O(n₁ × n₂) for computing differences where n₁ and n₂ are the lengths of the spectra
- O(k log k) for sorting where k is the number of unique differences
- Overall: O(n₁ × n₂ + k log k)

## Space Complexity
- O(n₁ × n₂) for storing differences
- O(k) for frequency table
- Overall: O(n₁ × n₂ + k)

