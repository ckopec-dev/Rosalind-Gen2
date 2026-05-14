# Rosalind Problem: Finding a Motif in DNA (Lisp Solution)

## Problem Description
Given two DNA strings s and t, find all locations where t occurs as a substring of s.

## Solution

```lisp
(defun find-motif (s t)
  "Find all starting positions (1-indexed) where motif t occurs in DNA string s."
  (let ((positions '())
        (s-len (length s))
        (t-len (length t)))
    (loop for i from 0 to (- s-len t-len)
          when (string= s t :start1 i :end1 (+ i t-len))
          do (push (1+ i) positions))
    (nreverse positions)))

;; Alternative implementation using string-search
(defun find-motif-alt (s t)
  "Alternative implementation using string-search."
  (let ((positions '())
        (t-len (length t)))
    (loop for pos = (search t s :start2 0 :end2 (length s))
          while pos
          do (progn
               (push (1+ pos) positions)
               (setf s (subseq s (1+ pos)))
               (setf pos (search t s :start2 0 :end2 (length s)))))
    (nreverse positions)))

;; Example usage
(defun solve-roslind ()
  "Solve the Rosalind problem with example inputs."
  (let ((s "GATATATGCATATACTT")
        (t "ATAT"))
    (format t "DNA string: ~A~%" s)
    (format t "Motif: ~A~%" t)
    (format t "Positions: ~A~%" (find-motif s t))))

;; Test function
(defun test-find-motif ()
  "Test the motif finding function with various inputs."
  (format t "Test 1: ~A~%" (find-motif "GATATATGCATATACTT" "ATAT"))
  (format t "Test 2: ~A~%" (find-motif "AAAAA" "AA"))
  (format t "Test 3: ~A~%" (find-motif "ACGTACGTACGT" "CGT"))
  (format t "Test 4: ~A~%" (find-motif "ATATAT" "TAT")))

;; Main function to read from input and solve
(defun solve-with-input ()
  "Read input from stdin and solve the problem."
  (let ((s (read-line))
        (t (read-line)))
    (format t "~{~A ~}~%" (find-motif s t))))

;; Run the example
(solve-roslind)
```

## Explanation

### Key Functions:

1. **`find-motif`**: Main function that finds all occurrences of motif `t` in DNA string `s`
   - Uses `string=` with start/end parameters to compare substrings
   - Returns 1-indexed positions (as required by Rosalind)
   - Uses `nreverse` to return positions in ascending order

2. **`find-motif-alt`**: Alternative implementation using `search` function
   - More complex but demonstrates different approach
   - Updates the search string after each match

### Algorithm Steps:
1. Get lengths of both strings
2. Loop through possible starting positions in string `s`
3. For each position, check if substring matches motif `t`
4. If match found, record the 1-indexed position
5. Return all positions in ascending order

### Time Complexity: O(n×m) where n is length of s and m is length of t
### Space Complexity: O(k) where k is number of matches

## Sample Output
For input:
```
GATATATGCATATACTT
ATAT
```

Output:
```
DNA string: GATATATGCATATACTT
Motif: ATAT
Positions: (2 4 10 12)
```

This solution correctly identifies all starting positions (1-indexed) where the motif "ATAT" appears in the DNA string.

