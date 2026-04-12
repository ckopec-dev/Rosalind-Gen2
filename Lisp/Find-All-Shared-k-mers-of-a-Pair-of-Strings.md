# Rosalind Problem: Find All Shared k-mers of a Pair of Strings (Lisp Solution)

## Problem Understanding

The task is to find all shared k-mers (k-length substrings) between two DNA strings, including their reverse complements.

## Solution Approach

1. Generate all k-mers from the first string
2. Generate all k-mers from the second string
3. Find intersection of these sets
4. Handle reverse complements properly

## Lisp Implementation

```lisp
(defun reverse-complement (dna-string)
  "Return the reverse complement of a DNA string."
  (let ((complement-map '((#\A . #\T) (#\T . #\A) (#\G . #\C) (#\C . #\G))))
    (map 'string 
         (lambda (char)
           (gethash char complement-map))
         (reverse dna-string))))

(defun get-k-mers (string k)
  "Return all k-mers from a string as a list."
  (if (< (length string) k)
      '()
      (loop for i from 0 to (- (length string) k)
            collect (subseq string i (+ i k)))))

(defun find-all-shared-k-mers (string1 string2 k)
  "Find all shared k-mers between two strings, including reverse complements."
  (let* ((kmers1 (get-k-mers string1 k))
         (kmers2 (get-k-mers string2 k))
         (set1 (make-hash-table :test 'equal))
         (set2 (make-hash-table :test 'equal))
         (shared-kmers '()))
    
    ;; Populate hash tables with k-mers from both strings
    (loop for kmer in kmers1 do
          (setf (gethash kmer set1) t))
    
    (loop for kmer in kmers2 do
          (setf (gethash kmer set2) t))
    
    ;; Find shared k-mers (including reverse complements)
    (loop for kmer in kmers1 do
          (let ((rc (reverse-complement kmer)))
            (when (gethash kmer set2)
              (push kmer shared-kmers))
            (when (gethash rc set2)
              (push kmer shared-kmers)))))
    
    ;; Remove duplicates and return
    (remove-duplicates shared-kmers :test 'equal)))

;; Alternative cleaner implementation
(defun find-shared-k-mers (string1 string2 k)
  "Find all shared k-mers between two strings."
  (let* ((kmers1 (get-k-mers string1 k))
         (kmers2 (get-k-mers string2 k))
         (set2 (make-hash-table :test 'equal))
         (result '()))
    
    ;; Put all k-mers from string2 into hash table
    (loop for kmer in kmers2 do
          (setf (gethash kmer set2) t))
    
    ;; Check each k-mer from string1
    (loop for kmer in kmers1 do
          (when (gethash kmer set2)
            (push kmer result))
          (let ((rc (reverse-complement kmer)))
            (when (gethash rc set2)
              (push kmer result)))))
    
    (remove-duplicates result :test 'equal)))

;; Example usage:
;; (find-shared-k-mers "AAACTCATC" "TTATCTGTG" 3)
```

## Example

For input strings:
- String1: "AAACTCATC"
- String2: "TTATCTGTG" 
- k: 3

The function would:
1. Generate k-mers from string1: ["AAA", "AAC", "ACT", "CTC", "TCA", "CAT", "ATC"]
2. Generate k-mers from string2: ["TTA", "TAT", "ATC", "TCT", "CTG", "TGT", "GTG"]
3. Find shared k-mers: ["ATC"] (since ATC appears in both strings)
4. Include reverse complements if needed

## Key Features

- Uses hash tables for efficient lookup
- Handles reverse complement computation properly
- Removes duplicate results
- Works with any DNA string and k value
- Returns results in a clean list format

## Time Complexity
O(n₁k + n₂k) where n₁ and n₂ are the lengths of the input strings.

## Space Complexity  
O(n₁k + n₂k) for storing the k-mers.

