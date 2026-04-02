# Rosalind Problem: Genome Assembly with Perfect Coverage and Repeats

## Problem Understanding

This problem asks us to assemble a genome from overlapping reads when we have perfect coverage and there may be repeats. We need to find the shortest superstring that contains all given reads as substrings.

## Approach

We'll use a greedy approach to find the shortest superstring:
1. Find the maximum overlap between any two reads
2. Merge the two reads with maximum overlap
3. Repeat until we have one superstring

## Solution in Lisp

```lisp
(defun overlap (read1 read2)
  "Find the maximum overlap between read1 and read2.
   Returns the length of the overlap, or 0 if no overlap."
  (let ((len1 (length read1))
        (len2 (length read2))
        (max-overlap 0))
    (loop for i from 1 to (min len1 len2) do
          (when (string= (subseq read1 (- len1 i)) (subseq read2 0 i))
            (setf max-overlap i)))
    max-overlap))

(defun merge-reads (read1 read2 overlap-len)
  "Merge two reads with given overlap.
   Returns the merged string."
  (if (= overlap-len 0)
      (concatenate 'string read1 read2)
      (concatenate 'string read1 (subseq read2 overlap-len))))

(defun find-max-overlap (reads)
  "Find the pair of reads with maximum overlap.
   Returns (read1-index read2-index overlap-length)."
  (let ((max-overlap 0)
        (best-pair nil))
    (loop for i from 0 to (1- (length reads)) do
          (loop for j from 0 to (1- (length reads)) do
                (when (/= i j)
                  (let ((ov (overlap (aref reads i) (aref reads j))))
                    (when (> ov max-overlap)
                      (setf max-overlap ov
                            best-pair (list i j ov)))))))
    best-pair))

(defun shortest-superstring (reads)
  "Find the shortest superstring containing all reads as substrings."
  (if (= (length reads) 1)
      (aref reads 0)
      (let ((read-array (copy-seq reads)))
        (loop while (> (length read-array) 1) do
              (let ((pair (find-max-overlap read-array)))
                (if pair
                    (let* ((i (first pair))
                           (j (second pair))
                           (overlap-len (third pair))
                           (read1 (aref read-array i))
                           (read2 (aref read-array j))
                           (merged (merge-reads read1 read2 overlap-len)))
                      (setf read-array 
                            (remove-if (lambda (x) (or (string= x read1) (string= x read2)))
                                       read-array))
                      (vector-push-extend merged read-array))
                    (return))))
        (aref read-array 0))))

;; Alternative implementation using a more efficient approach
(defun genome-assembly-perfect-coverage (reads)
  "Main function to solve genome assembly with perfect coverage."
  (shortest-superstring reads))

;; Example usage:
;; (genome-assembly-perfect-coverage 
;;  #("ATTAGACCTG" "CCTGCCGGAA" "TACCTGCCGG" "GCCGGAAATC" "CCGGAAATCA" "GGAAATCATC"))
```

## Explanation

The solution works as follows:

1. **Overlap Function**: Calculates the maximum overlap between two strings by checking if the suffix of one string matches the prefix of another.

2. **Merge Reads**: Combines two reads by merging them at the maximum overlap point.

3. **Find Maximum Overlap**: Iterates through all pairs of reads to find the pair with maximum overlap.

4. **Shortest Superstring**: Uses a greedy approach:
   - Find the pair of reads with maximum overlap
   - Merge them into one string
   - Remove the original two strings from consideration
   - Repeat until only one string remains

## Time Complexity
- The algorithm has O(n³m) time complexity where n is the number of reads and m is the average read length
- This is acceptable for the given constraints

## Space Complexity
- O(nm) for storing the reads and intermediate results

## Example

For input reads: `["ATTAGACCTG", "CCTGCCGGAA", "TACCTGCCGG", "GCCGGAAATC", "CCGGAAATCA", "GGAAATCATC"]`

The algorithm would find the shortest superstring that contains all these reads as substrings, accounting for overlaps and repeats in the genome.

