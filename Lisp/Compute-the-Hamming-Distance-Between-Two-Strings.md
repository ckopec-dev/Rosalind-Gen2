# Rosalind Problem: Compute the Hamming Distance Between Two Strings

## Problem Description
The Hamming distance between two strings of equal length is the number of positions at which the corresponding symbols are different. Given two strings of equal length, we need to compute their Hamming distance.

## Solution in Lisp

```lisp
(defun hamming-distance (str1 str2)
  "Compute the Hamming distance between two strings of equal length."
  (if (not (= (length str1) (length str2)))
      (error "Strings must have equal length")
      (let ((distance 0))
        (loop for i from 0 below (length str1)
              when (not (char= (char str1 i) (char str2 i)))
              do (incf distance))
        distance)))

;; Alternative implementation using recursion
(defun hamming-distance-rec (str1 str2)
  "Recursive implementation of Hamming distance."
  (if (not (= (length str1) (length str2)))
      (error "Strings must have equal length")
      (if (zerop (length str1))
          0
          (let ((current-char-diff (if (char= (char str1 0) (char str2 0))
                                       0
                                       1)))
            (+ current-char-diff
               (hamming-distance-rec 
                (subseq str1 1) 
                (subseq str2 1)))))))

;; Example usage:
;; (hamming-distance "GAGCCTACTAACGGGAT" "CATCGGTATCGCCCGTT")
;; Returns: 7

;; (hamming-distance-rec "GAGCCTACTAACGGGAT" "CATCGGTATCGCCCGTT")
;; Returns: 7
```

## Explanation

The solution provides two implementations:

1. **Iterative approach (`hamming-distance`)**: 
   - Uses a loop to iterate through each character position
   - Compares characters at each position using `char=` function
   - Increments the distance counter when characters don't match
   - Includes error checking for strings of unequal length

2. **Recursive approach (`hamming-distance-rec`)**:
   - Base case: if strings are empty, return 0
   - Compares first characters and adds 1 if they differ
   - Recursively processes the remaining substrings

## Test Example

For the sample input:
- String 1: `"GAGCCTACTAACGGGAT"`
- String 2: `"CATCGGTATCGCCCGTT"`

The Hamming distance is 7, as there are 7 positions where the characters differ.

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the strings
- **Space Complexity**: O(1) for iterative, O(n) for recursive due to call stack

The iterative solution is generally preferred as it doesn't risk stack overflow for very long strings.

