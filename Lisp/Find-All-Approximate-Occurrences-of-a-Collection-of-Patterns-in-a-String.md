# Rosalind Problem: Find All Approximate Occurrences of a Collection of Patterns in a String

## Problem Description
Given a string Text, a collection of strings Patterns, and an integer d, we need to find all positions where each pattern appears in Text with at most d mismatches.

## Solution in Lisp

```lisp
(defun hamming-distance (s1 s2)
  "Calculate the Hamming distance between two strings of equal length."
  (if (null s1)
      0
      (+ (if (char= (first s1) (first s2)) 0 1)
         (hamming-distance (rest s1) (rest s2)))))

(defun approximate-pattern-count (text pattern d)
  "Count occurrences of pattern in text with at most d mismatches."
  (let ((count 0)
        (pattern-length (length pattern))
        (text-length (length text)))
    (loop for i from 0 to (- text-length pattern-length)
          do (let ((substring (subseq text i (+ i pattern-length))))
               (when (<= (hamming-distance pattern substring) d)
                 (incf count))))
    count))

(defun find-all-approximate-occurrences (text patterns d)
  "Find all approximate occurrences of patterns in text with at most d mismatches."
  (let ((results '()))
    (loop for pattern in patterns
          do (let ((occurrences '()))
               (loop for i from 0 to (- (length text) (length pattern))
                     do (let ((substring (subseq text i (+ i (length pattern)))))
                          (when (<= (hamming-distance pattern substring) d)
                            (push i occurrences))))
               (push (list pattern (nreverse occurrences)) results)))
    (nreverse results)))

;; Alternative implementation using a more efficient approach
(defun find-approximate-occurrences (text patterns d)
  "Find all approximate occurrences of patterns in text with at most d mismatches."
  (let ((all-occurrences '()))
    (loop for pattern in patterns
          do (let ((pattern-occurrences '()))
               (loop for i from 0 to (- (length text) (length pattern))
                     do (let ((substring (subseq text i (+ i (length pattern)))))
                          (when (<= (hamming-distance pattern substring) d)
                            (push i pattern-occurrences))))
               (push (cons pattern (nreverse pattern-occurrences)) all-occurrences)))
    (nreverse all-occurrences)))

;; Example usage:
;; (find-approximate-occurrences "ACAACTATGCATACTATCGGGAACTATCCT" '("ACT" "ATT" "GGA") 1)
```

## Explanation

The solution consists of several key functions:

1. **`hamming-distance`**: Calculates the Hamming distance between two strings of equal length by counting the number of positions where characters differ.

2. **`approximate-pattern-count`**: Counts how many times a specific pattern appears in text with at most d mismatches.

3. **`find-all-approximate-occurrences`**: Main function that finds all approximate occurrences for each pattern in the collection.

## Key Features

- Handles multiple patterns in a collection
- Uses Hamming distance to measure mismatches
- Returns positions (0-indexed) where patterns occur with at most d mismatches
- Efficiently checks all possible substrings of the text

## Time Complexity
- O(n × m × k) where n is the length of text, m is the average length of patterns, and k is the number of patterns
- For each pattern, we check all possible positions in the text

## Space Complexity
- O(p) where p is the total number of approximate occurrences found

This solution efficiently handles the Rosalind problem requirements for finding approximate occurrences of multiple patterns in a string with a given maximum number of mismatches.

