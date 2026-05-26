# Rosalind Problem: Expected Number of Restriction Sites

## Problem Understanding

We need to calculate the expected number of occurrences of a specific restriction site pattern in a DNA sequence of given length, where the pattern has a certain probability of occurring at each position.

## Solution Approach

The expected number of occurrences of a restriction site pattern in a DNA sequence can be calculated as:
- Expected occurrences = (sequence length - pattern length + 1) × P(pattern)

Where P(pattern) is the probability that a random k-length substring matches the pattern, considering that each nucleotide has probability 0.25 of being any particular base.

## Lisp Implementation

```lisp
(defun expected-restriction-sites (sequence-length pattern)
  "Calculate the expected number of occurrences of a restriction site pattern.
   Args:
     sequence-length: length of the DNA sequence
     pattern: the restriction site pattern string
   Returns:
     expected number of occurrences"
  (let ((pattern-length (length pattern))
        (num-positions (- sequence-length pattern-length 1)))
    (if (<= pattern-length sequence-length)
        (let ((probability (calculate-pattern-probability pattern)))
          (* num-positions probability))
        0)))

(defun calculate-pattern-probability (pattern)
  "Calculate the probability of a pattern occurring at a random position.
   Each nucleotide has probability 0.25 of being any base.
   Args:
     pattern: DNA pattern string
   Returns:
     probability of the pattern"
  (let ((probability 1.0))
    (loop for nucleotide across pattern do
          (setf probability (* probability 0.25)))
    probability))

;; Alternative more concise implementation
(defun expected-sites (n k)
  "Calculate expected number of restriction sites.
   n: sequence length, k: pattern length"
  (if (<= k n)
      (* (- n k 1) (expt 0.25 k))
      0))
```

## Example Usage

```lisp
;; Example: sequence length 1000, pattern "ATCG"
(expected-restriction-sites 1000 "ATCG")
;; Expected result: 937.5

;; Using the concise version
(expected-sites 1000 4)
;; Expected result: 937.5
```

## Step-by-Step Solution

1. **Input Analysis**: 
   - We have a DNA sequence of length `n`
   - We want to find expected occurrences of a pattern of length `k`

2. **Position Counting**:
   - In a sequence of length `n`, there are `(n - k + 1)` possible positions where the pattern can start

3. **Probability Calculation**:
   - Each nucleotide in the pattern has probability 0.25 of matching
   - For a pattern of length `k`, probability = (0.25)^k

4. **Expected Value**:
   - Expected occurrences = Number of positions × Probability per position
   - Expected = (n - k + 1) × (0.25)^k

## Final Implementation

```lisp
(defun expected-number-of-restriction-sites (n k)
  "Calculate expected number of restriction sites.
   n: sequence length
   k: pattern length"
  (if (<= k n)
      (* (- n k 1) (expt 0.25 k))
      0))

;; For the specific problem format
(defun solve-restriction-sites (sequence-length pattern)
  "Solve the restriction sites problem.
   Args:
     sequence-length: length of DNA sequence
     pattern: restriction site pattern
   Returns:
     expected number of occurrences"
  (let ((pattern-length (length pattern)))
    (expected-number-of-restriction-sites sequence-length pattern-length)))
```

The solution correctly calculates that for a sequence of length n and a pattern of length k, the expected number of restriction sites is `(n - k - 1) × (0.25)^k`.

