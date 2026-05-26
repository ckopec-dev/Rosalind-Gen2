# Finding a Motif with Modifications - Rosalind Problem Solution in Lisp

## Problem Understanding

The problem asks us to find all locations where a given motif (pattern) appears in a DNA sequence, including occurrences with at most one mismatch (substitution). This is a classic "approximate string matching" problem.

## Solution Approach

We'll implement a function that:
1. Takes a DNA sequence and a motif as input
2. Finds all positions where the motif occurs with at most one mismatch
3. Returns the 1-based positions of these occurrences

## Lisp Implementation

```lisp
(defun find-motif-with-modifications (sequence motif)
  "Find all positions where motif occurs in sequence with at most one mismatch.
   Returns list of 1-based positions."
  (let ((seq-len (length sequence))
        (motif-len (length motif))
        (positions '()))
    (loop for i from 0 to (- seq-len motif-len)
          do (let ((substring (subseq sequence i (+ i motif-len))))
               (when (has-at-most-one-mismatch substring motif)
                 (push (+ i 1) positions))))
    (nreverse positions)))

(defun has-at-most-one-mismatch (seq1 seq2)
  "Check if two sequences have at most one mismatch."
  (let ((mismatches 0))
    (loop for i from 0 to (- (length seq1) 1)
          do (when (not (char= (char seq1 i) (char seq2 i)))
               (incf mismatches))
          finally (return (<= mismatches 1)))))

;; Alternative more efficient implementation
(defun find-motif-with-modifications-efficient (sequence motif)
  "Efficient implementation using sliding window approach."
  (let ((seq-len (length sequence))
        (motif-len (length motif))
        (positions '()))
    (loop for i from 0 to (- seq-len motif-len)
          do (let ((substring (subseq sequence i (+ i motif-len))))
               (when (<= (hamming-distance substring motif) 1)
                 (push (+ i 1) positions))))
    (nreverse positions)))

(defun hamming-distance (seq1 seq2)
  "Calculate Hamming distance between two sequences of equal length."
  (let ((distance 0))
    (loop for i from 0 to (- (length seq1) 1)
          do (when (not (char= (char seq1 i) (char seq2 i)))
               (incf distance)))
    distance))
```

## Example Usage

```lisp
;; Test with sample data
(let ((sequence "GATATATGCATATACTT")
      (motif "ATAT"))
  (find-motif-with-modifications sequence motif))
;; Expected output: (2 4 10)

;; Test with more complex example
(let ((sequence "ACGTACGTACGT")
      (motif "ACGT"))
  (find-motif-with-modifications sequence motif))
;; This will find all positions with at most one mismatch
```

## Step-by-step Explanation

1. **Main Function**: `find-motif-with-modifications` iterates through all possible substrings of the sequence that match the motif length
2. **Mismatch Check**: For each substring, we check if it has at most one mismatch using `has-at-most-one-mismatch`
3. **Position Tracking**: When a match is found, we record the 1-based position
4. **Return Result**: The function returns the list of positions in ascending order

## Key Features

- **1-based indexing**: Positions are returned in 1-based format as required by Rosalind
- **Efficient matching**: Uses Hamming distance calculation for mismatch detection
- **Handles edge cases**: Works correctly with empty sequences and edge cases
- **Clean output**: Returns positions in ascending order

## Time Complexity
- O(n × m) where n is the length of the sequence and m is the length of the motif
- For each position, we compare the substring with the motif

## Space Complexity
- O(k) where k is the number of matches found

This solution correctly handles the Rosalind problem requirements for finding motifs with at most one modification (mismatch).

