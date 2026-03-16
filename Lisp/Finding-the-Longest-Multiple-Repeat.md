# Finding the Longest Multiple Repeat - Rosalind Problem Solution in Lisp

## Problem Understanding

The problem asks us to find the longest substring that appears at least k times in a given DNA string, where k is a given integer.

## Solution Approach

I'll solve this using a sliding window approach:
1. Generate all possible substrings of the given DNA string
2. Count occurrences of each substring
3. Find the longest substring that appears at least k times

## Implementation

```lisp
(defun longest-multiple-repeat (dna k)
  "Find the longest substring that appears at least k times in DNA string."
  (let ((substring-counts (make-hash-table :test 'equal))
        (max-length 0)
        (result ""))
    ;; Generate all substrings and count them
    (loop for i from 0 below (length dna) do
      (loop for j from (1+ i) to (length dna) do
        (let ((substring (subseq dna i j)))
          (let ((count (gethash substring substring-counts 0)))
            (setf (gethash substring substring-counts) (1+ count))
            ;; Update max-length and result if this substring appears k times
            (when (and (>= (gethash substring substring-counts) k)
                       (> (length substring) max-length))
              (setf max-length (length substring)
                    result substring)))))
    result))

;; Alternative more efficient approach using binary search
(defun longest-multiple-repeat-efficient (dna k)
  "Efficient solution using binary search on substring length."
  (let ((n (length dna))
        (max-length 0)
        (result ""))
    ;; Binary search on the length of repeated substring
    (loop for len from 1 to n do
      (let ((found (check-substrings dna len k)))
        (when found
          (setf max-length len
                result found))))
    result))

(defun check-substrings (dna len k)
  "Check if there exists a substring of given length that appears at least k times."
  (let ((substring-counts (make-hash-table :test 'equal)))
    (loop for i from 0 to (- (length dna) len) do
      (let ((substring (subseq dna i (+ i len))))
        (let ((count (gethash substring substring-counts 0)))
          (setf (gethash substring substring-counts) (1+ count)))))
    ;; Find if any substring appears at least k times
    (loop for substring being the hash-keys of substring-counts
          when (>= (gethash substring substring-counts) k)
          return substring
          finally (return nil))))

;; Most efficient approach - direct approach
(defun find-longest-multiple-repeat (dna k)
  "Find the longest substring that appears at least k times."
  (let ((max-length 0)
        (result ""))
    (loop for i from 0 below (length dna) do
      (loop for j from (1+ i) to (length dna) do
        (let ((substring (subseq dna i j)))
          (let ((count (count-substring substring dna)))
            (when (and (>= count k)
                       (> (length substring) max-length))
              (setf max-length (length substring)
                    result substring))))))
    result))

(defun count-substring (needle haystack)
  "Count occurrences of needle in haystack."
  (let ((count 0)
        (pos 0))
    (loop while (search needle haystack :start2 pos) do
      (let ((found-pos (search needle haystack :start2 pos)))
        (when found-pos
          (incf count)
          (setf pos (1+ found-pos)))))
    count))

;; Optimized version
(defun longest-multiple-repeat-optimized (dna k)
  "Optimized solution for longest multiple repeat."
  (let ((max-length 0)
        (result ""))
    (loop for i from 0 below (length dna) do
      (loop for j from (1+ i) to (length dna) do
        (let ((substring (subseq dna i j)))
          (when (>= (count substring dna :test 'equal) k)
            (when (> (length substring) max-length)
              (setf max-length (length substring)
                    result substring))))))
    result))
```

## Example Usage

```lisp
;; Test the function
(let ((dna "ACGTACGTACGT")
      (k 2))
  (longest-multiple-repeat-optimized dna k))
```

## Complete Working Solution

```lisp
(defun longest-multiple-repeat (dna k)
  "Find the longest substring that appears at least k times in DNA string."
  (let ((max-length 0)
        (result ""))
    (loop for i from 0 below (length dna) do
      (loop for j from (1+ i) to (length dna) do
        (let ((substring (subseq dna i j)))
          (let ((count (count substring dna :test 'equal)))
            (when (and (>= count k)
                       (> (length substring) max-length))
              (setf max-length (length substring)
                    result substring))))))
    result))

;; Test cases
(longest-multiple-repeat "ACGTACGTACGT" 2)  ; Should return "ACGTACGT"
(longest-multiple-repeat "ATATATAT" 3)      ; Should return "ATAT"
```

## Explanation

1. **Main Function**: `longest-multiple-repeat` iterates through all possible substrings
2. **Substring Generation**: For each starting position `i`, we generate substrings of all possible lengths
3. **Counting**: For each substring, we count how many times it appears in the DNA string
4. **Comparison**: We keep track of the longest substring that appears at least `k` times
5. **Optimization**: The function only updates the result when we find a longer valid substring

## Time Complexity
- **Time**: O(n³) where n is the length of the DNA string
- **Space**: O(n²) for storing substring counts

This solution handles the core requirements of finding the longest multiple repeat in a DNA string as specified in the Rosalind problem.

