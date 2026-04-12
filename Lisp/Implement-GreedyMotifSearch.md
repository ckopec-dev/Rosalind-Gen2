# Rosalind Problem: Implement_GreedyMotifSearch in Lisp

## Problem Description
Implement the Greedy Motif Search algorithm to find the best motif (k-mer) in each DNA sequence that collectively forms a motif matrix with minimum score.

## Solution

```lisp
(defun profile-matrix (motifs)
  "Calculate the profile matrix for a list of motifs"
  (let ((k (length (first motifs)))
        (profile (make-array '(4 k) :initial-element 0)))
    (dolist (motif motifs)
      (loop for i from 0 below k
            for nucleotide across motif
            do (case nucleotide
                 (#\A (incf (aref profile 0 i)))
                 (#\C (incf (aref profile 1 i)))
                 (#\G (incf (aref profile 2 i)))
                 (#\T (incf (aref profile 3 i)))))
    profile))

(defun score-matrix (profile)
  "Calculate the score of a profile matrix"
  (let ((k (array-dimension profile 1))
        (score 0))
    (loop for i from 0 below k
          for max-value = (apply #'max (loop for j from 0 below 4
                                            collect (aref profile j i)))
          do (incf score (- 4 max-value)))
    score))

(defun most-probable-kmer (text profile k)
  "Find the most probable k-mer in text given the profile"
  (let ((best-kmer "")
        (best-score -1))
    (loop for i from 0 to (- (length text) k)
          for kmer = (subseq text i (+ i k))
          for score = (loop for j from 0 below k
                           for nucleotide across kmer
                           for profile-row = (case nucleotide
                                              (#\A 0)
                                              (#\C 1)
                                              (#\G 2)
                                              (#\T 3))
                           sum (log (aref profile profile-row j))
                           with total = 0
                           finally (return total))
          do (when (or (string= best-kmer "") (> score best-score))
               (setf best-kmer kmer
                     best-score score)))
    best-kmer))

(defun greedy-motif-search (dna k t)
  "Implement Greedy Motif Search algorithm"
  (let ((best-motifs '())
        (best-score most-positive-fixnum))
    ;; Initialize with first k-mers from each sequence
    (loop for i from 0 below t
          do (push (subseq (nth i dna) 0 k) best-motifs))
    (setf best-motifs (nreverse best-motifs))
    
    ;; Try all possible k-mers in the first sequence
    (loop for i from 0 to (- (length (first dna)) k)
          for motif = (subseq (first dna) i (+ i k))
          for motifs = (list motif)
          for j from 1 below t
          do (let ((profile (profile-matrix motifs)))
               (push (most-probable-kmer (nth j dna) profile k) motifs)
               (setf motifs (nreverse motifs)))
          do (let ((score (score-matrix (profile-matrix motifs))))
               (when (< score best-score)
                 (setf best-score score
                       best-motifs motifs))))
    best-motifs))

;; Alternative cleaner implementation
(defun greedy-motif-search-clean (dna k t)
  "Clean implementation of Greedy Motif Search"
  (let ((best-motifs '())
        (best-score most-positive-fixnum))
    ;; Try all k-mers from first sequence
    (loop for i from 0 to (- (length (first dna)) k)
          for motif = (subseq (first dna) i (+ i k))
          for motifs = (list motif)
          for j from 1 below t
          do (let ((profile (profile-matrix motifs)))
               (push (most-probable-kmer (nth j dna) profile k) motifs)
               (setf motifs (nreverse motifs)))
          do (let ((score (score-matrix (profile-matrix motifs))))
               (when (< score best-score)
                 (setf best-score score
                       best-motifs motifs))))
    best-motifs))

;; Helper function to calculate profile matrix properly
(defun calculate-profile (motifs)
  "Calculate profile matrix with pseudocounts"
  (let ((k (length (first motifs)))
        (profile (make-array '(4 k) :initial-element 1))) ; Pseudocounts
    (dolist (motif motifs)
      (loop for i from 0 below k
            for nucleotide across motif
            do (case nucleotide
                 (#\A (incf (aref profile 0 i)))
                 (#\C (incf (aref profile 1 i)))
                 (#\G (incf (aref profile 2 i)))
                 (#\T (incf (aref profile 3 i)))))
    profile))

(defun greedy-motif-search-with-pseudocounts (dna k t)
  "Greedy Motif Search with pseudocounts"
  (let ((best-motifs '())
        (best-score most-positive-fixnum))
    (loop for i from 0 to (- (length (first dna)) k)
          for motif = (subseq (first dna) i (+ i k))
          for motifs = (list motif)
          for j from 1 below t
          do (let ((profile (calculate-profile motifs)))
               (push (most-probable-kmer (nth j dna) profile k) motifs)
               (setf motifs (nreverse motifs)))
          do (let ((score (score-matrix (calculate-profile motifs))))
               (when (< score best-score)
                 (setf best-score score
                       best-motifs motifs))))
    best-motifs))
```

## Usage Example

```lisp
;; Example usage
(setq dna '("GGCGTTCAGGCA" "AAGAATCAGTCA" "CAAGGAGTTCCT" "CAGGTCACTTCC"))
(setq k 3)
(setq t 4)

(greedy-motif-search dna k t)
```

## Key Components

1. **Profile Matrix Calculation**: Computes the frequency of each nucleotide at each position
2. **Score Calculation**: Determines how good a motif is based on the profile
3. **Most Probable K-mer**: Finds the k-mer most likely to occur given a profile
4. **Greedy Search**: Iteratively builds motifs while trying all possibilities for the first sequence

## Time Complexity
- O(t * n * k * t * k) = O(t² * n * k²) where t is number of sequences, n is sequence length, and k is motif length

## Space Complexity
- O(t * k) for storing the motifs and profile matrix

This implementation follows the standard Greedy Motif Search algorithm as described in the Rosalind problem requirements.

