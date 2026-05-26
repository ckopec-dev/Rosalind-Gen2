# Rosalind Problem: Implement_DistanceBetweenPatternAndStrings

## Problem Description
The distance between a k-mer pattern and a collection of strings is the minimum Hamming distance between pattern and any k-mer in the strings.

## Solution in Lisp

```lisp
(defun hamming-distance (s1 s2)
  "Calculate the Hamming distance between two strings of equal length."
  (if (null s1)
      0
      (+ (if (char= (first s1) (first s2)) 0 1)
         (hamming-distance (rest s1) (rest s2)))))

(defun kmer-list (string k)
  "Generate all k-mers from a string."
  (if (<= (length string) k)
      (list string)
      (cons (subseq string 0 k)
            (kmer-list (rest string) k))))

(defun distance-between-pattern-strings (pattern strings)
  "Calculate the distance between a pattern and a collection of strings."
  (let ((k (length pattern))
        (min-distance most-positive-fixnum))
    (dolist (string strings)
      (let ((kmers (kmer-list string k)))
        (dolist (kmer kmers)
          (let ((distance (hamming-distance pattern kmer)))
            (when (< distance min-distance)
              (setf min-distance distance)))))
    min-distance))

;; Alternative more concise implementation
(defun distance-between-pattern-strings-concise (pattern strings)
  "Concise version using built-in functions."
  (let ((k (length pattern)))
    (apply #'min
           (mapcan (lambda (string)
                      (mapcar (lambda (kmer)
                                 (hamming-distance pattern kmer))
                               (kmer-list string k)))
                    strings))))

;; Example usage:
;; (distance-between-pattern-strings "AAA" '("TTACCTTAAC" "GATATCTGAC" "AAGAAGTGAT"))
```

## Explanation

This solution implements the distance between a pattern and a collection of strings by:

1. **`hamming-distance`**: Calculates the Hamming distance between two strings of equal length by comparing each character position.

2. **`kmer-list`**: Generates all k-mers (substrings of length k) from a given string.

3. **`distance-between-pattern-strings`**: 
   - For each string in the collection, generates all k-mers of length k
   - Calculates the Hamming distance between the pattern and each k-mer
   - Keeps track of the minimum distance found

4. **`distance-between-pattern-strings-concise`**: A more functional approach using `mapcan` and `apply` to achieve the same result more concisely.

## Time Complexity
- O(n × m × k) where n is the number of strings, m is the average length of strings, and k is the pattern length
- This is efficient for the typical input sizes in Rosalind problems

## Example
For pattern "AAA" and strings `("TTACCTTAAC", "GATATCTGAC", "AAGAAGTGAT")`:
- The minimum Hamming distance is 1 (between "AAA" and "AAG" in the third string)

