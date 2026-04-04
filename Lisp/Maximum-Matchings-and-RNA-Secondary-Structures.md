# Rosalind Problem: Maximum Matchings and RNA Secondary Structures

I'll solve this step by step using Lisp to find the maximum number of base pairings in an RNA sequence.

## Problem Understanding

We need to find the maximum number of base pairings in an RNA sequence where:
- A pairs with U
- C pairs with G
- Base pairs cannot cross each other (non-crossing matchings)
- We want to maximize the number of base pairs

This is a classic dynamic programming problem related to RNA secondary structure prediction.

## Solution Approach

I'll use dynamic programming with the following recurrence:
- For a substring from index i to j, we can either:
  1. Not pair the first base (max matchings in substring i+1 to j)
  2. Pair the first base with some base k (if they can pair) and add 1 + max matchings in i+1 to k-1 + k+1 to j

## Lisp Implementation

```lisp
(defun maximum-matchings (rna-sequence)
  "Calculate maximum number of base pairings in RNA sequence"
  (let ((n (length rna-sequence))
        (memo (make-hash-table :test 'equal)))
    (if (<= n 1)
        0
        (max-matchings-helper rna-sequence 0 (1- n) memo))))

(defun max-matchings-helper (rna-sequence i j memo)
  "Helper function for dynamic programming with memoization"
  (cond
    ;; Base cases
    ((>= i j) 0)
    ((= i j) 0)
    ;; Check memoization
    ((gethash (list i j) memo)
     (gethash (list i j) memo))
    ;; Recursive case
    (t
     (let ((result 0))
       ;; Option 1: don't pair the first base
       (let ((option1 (max-matchings-helper rna-sequence (1+ i) j memo)))
         (setf result (max result option1)))
       
       ;; Option 2: try pairing first base with each possible base
       (loop for k from (1+ i) to j do
             (when (can-pair (char rna-sequence i) (char rna-sequence k))
               (let ((option2 (+ 1
                                (max-matchings-helper rna-sequence (1+ i) (1- k) memo)
                                (max-matchings-helper rna-sequence (1+ k) j memo))))
                 (setf result (max result option2)))))
       
       ;; Store in memo table
       (setf (gethash (list i j) memo) result)
       result)))))

(defun can-pair (base1 base2)
  "Check if two RNA bases can pair"
  (or (and (char= base1 #\A) (char= base2 #\U))
      (and (char= base1 #\U) (char= base2 #\A))
      (and (char= base1 #\C) (char= base2 #\G))
      (and (char= base1 #\G) (char= base2 #\C))))

;; Alternative implementation using more efficient approach
(defun maximum-matchings-efficient (rna-sequence)
  "More efficient implementation using dynamic programming table"
  (let* ((n (length rna-sequence))
         (dp (make-array (list n n) :initial-element 0)))
    (if (<= n 1)
        0
        (progn
          ;; Fill the DP table
          (loop for l from 2 to n do
                (loop for i from 0 to (- n l) do
                      (let ((j (+ i l -1)))
                        ;; Option 1: don't pair i
                        (setf (aref dp i j) (aref dp (1+ i) j))
                        
                        ;; Option 2: try pairing i with k
                        (loop for k from (1+ i) to j do
                              (when (can-pair (char rna-sequence i) (char rna-sequence k))
                                (let ((val (+ 1
                                             (if (< i (1- k)) (aref dp (1+ i) (1- k)) 0)
                                             (if (< k j) (aref dp (1+ k) j) 0))))
                                  (setf (aref dp i j) (max (aref dp i j) val))))))))
          (aref dp 0 (1- n))))))

;; Test with example
(defun test-maximum-matchings ()
  "Test the function with example RNA sequences"
  (let ((test-sequence "AUCG"))
    (format t "RNA sequence: ~A~%" test-sequence)
    (format t "Maximum matchings: ~A~%" (maximum-matchings test-sequence))
    (format t "Maximum matchings (efficient): ~A~%" (maximum-matchings-efficient test-sequence))))

;; Run the test
(test-maximum-matchings)
```

## Example Walkthrough

For the RNA sequence "AUCG":

1. We can pair A with U (positions 0 and 2)
2. We can pair C with G (positions 1 and 3)
3. But we cannot pair A with G or C with U (no valid pairing)
4. The maximum matchings would be 2 (A-U and C-G)

## Key Points

1. **Base Pairing Rules**: A↔U, C↔G
2. **Non-crossing**: Base pairs cannot cross each other
3. **Dynamic Programming**: We use memoization or a 2D DP table
4. **Time Complexity**: O(n³) where n is the length of the sequence
5. **Space Complexity**: O(n²) for the DP table

## Usage

```lisp
;; To solve a specific RNA sequence
(maximum-matchings-efficient "AUCGCUAGCUAGCU")

;; For the exact problem requirements
(maximum-matchings-efficient "AUCG")
```

This solution handles the core dynamic programming approach needed for RNA secondary structure prediction, finding the maximum number of non-crossing base pairings in an RNA sequence.

