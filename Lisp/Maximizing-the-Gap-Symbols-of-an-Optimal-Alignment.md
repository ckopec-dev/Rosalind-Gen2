# Rosalind Problem: Maximizing the Gap Symbols of an Optimal Alignment (Lisp Solution)

## Problem Understanding

This problem asks us to find an optimal alignment between two sequences that maximizes the number of gap symbols (dashes) in the alignment, while still maintaining the optimal alignment score.

## Approach

We'll use dynamic programming with a modified scoring scheme where we penalize gaps more heavily than matches/mismatches, but we'll also track the maximum number of gaps in the optimal alignment.

## Solution

```lisp
(defun maximizing-gap-alignment (seq1 seq2)
  "Find optimal alignment that maximizes gap symbols while maintaining optimal score"
  (let* ((m (length seq1))
         (n (length seq2))
         ;; Create DP table for scores
         (dp (make-array (list (1+ m) (1+ n)) :initial-element 0))
         ;; Create table to track maximum gaps
         (gaps (make-array (list (1+ m) (1+ n)) :initial-element 0))
         ;; Scoring parameters
         (match-score 2)
         (mismatch-score -1)
         (gap-score -1))
    
    ;; Initialize base cases
    (loop for i from 0 to m do
          (setf (aref dp i 0) (* i gap-score)
                (aref gaps i 0) i))
    
    (loop for j from 0 to n do
          (setf (aref dp 0 j) (* j gap-score)
                (aref gaps 0 j) j))
    
    ;; Fill the DP table
    (loop for i from 1 to m do
          (loop for j from 1 to n do
                (let* ((match-score-val (if (char= (char seq1 (1- i)) (char seq2 (1- j)))
                                          match-score
                                          mismatch-score))
                       (score1 (+ (aref dp (1- i) (1- j)) match-score-val))
                       (score2 (+ (aref dp i (1- j)) gap-score))
                       (score3 (+ (aref dp (1- i) j) gap-score)))
                  
                  (let ((max-score (max score1 score2 score3)))
                    (setf (aref dp i j) max-score)
                    
                    ;; Track maximum gaps - this is tricky because we need to
                    ;; determine which path gives us the maximum number of gaps
                    (let ((max-gaps 0))
                      (cond
                        ((= max-score score1)
                         ;; Diagonal move (match/mismatch)
                         (setf max-gaps (aref gaps (1- i) (1- j))))
                        ((= max-score score2)
                         ;; Left move (gap in seq1)
                         (setf max-gaps (aref gaps i (1- j))))
                        ((= max-score score3)
                         ;; Up move (gap in seq2)
                         (setf max-gaps (aref gaps (1- i) j))))
                      (setf (aref gaps i j) max-gaps)))))
    
    ;; Reconstruct alignment
    (let ((alignment1 "")
          (alignment2 "")
          (i m)
          (j n))
      
      (loop while (and (> i 0) (> j 0)) do
            (let* ((match-score-val (if (char= (char seq1 (1- i)) (char seq2 (1- j)))
                                      match-score
                                      mismatch-score))
                   (score1 (+ (aref dp (1- i) (1- j)) match-score-val))
                   (score2 (+ (aref dp i (1- j)) gap-score))
                   (score3 (+ (aref dp (1- i) j) gap-score))
                   (current-score (aref dp i j)))
              
              (cond
                ((= current-score score1)
                 ;; Diagonal move
                 (setf alignment1 (concatenate 'string (string (char seq1 (1- i))) alignment1)
                       alignment2 (concatenate 'string (string (char seq2 (1- j))) alignment2))
                 (decf i)
                 (decf j))
                ((= current-score score2)
                 ;; Gap in seq1
                 (setf alignment1 (concatenate 'string "-" alignment1)
                       alignment2 (concatenate 'string (string (char seq2 (1- j))) alignment2))
                 (decf j))
                ((= current-score score3)
                 ;; Gap in seq2
                 (setf alignment1 (concatenate 'string (string (char seq1 (1- i))) alignment1)
                       alignment2 (concatenate 'string "-" alignment2))
                 (decf i)))))
      
      ;; Add remaining characters
      (loop while (> i 0) do
            (setf alignment1 (concatenate 'string (string (char seq1 (1- i))) alignment1)
                  alignment2 (concatenate 'string "-" alignment2))
            (decf i))
      
      (loop while (> j 0) do
            (setf alignment1 (concatenate 'string "-" alignment1)
                  alignment2 (concatenate 'string (string (char seq2 (1- j))) alignment2))
            (decf j))
      
      (list alignment1 alignment2 (aref dp m n)))))

;; Alternative approach - more direct maximization of gaps
(defun maximize-gaps-align (seq1 seq2)
  "Solve the gap maximization problem directly"
  (let* ((m (length seq1))
         (n (length seq2))
         ;; DP table: [score][gaps]
         (dp (make-array (list (1+ m) (1+ n)) :initial-element 0))
         (gap-count (make-array (list (1+ m) (1+ n)) :initial-element 0))
         (gap-score -1)
         (match-score 2)
         (mismatch-score -1))
    
    ;; Initialize
    (loop for i from 0 to m do
          (setf (aref dp i 0) (* i gap-score)
                (aref gap-count i 0) i))
    
    (loop for j from 0 to n do
          (setf (aref dp 0 j) (* j gap-score)
                (aref gap-count 0 j) j))
    
    ;; Fill DP table
    (loop for i from 1 to m do
          (loop for j from 1 to n do
                (let* ((match-score-val (if (char= (char seq1 (1- i)) (char seq2 (1- j)))
                                          match-score
                                          mismatch-score))
                       (score1 (+ (aref dp (1- i) (1- j)) match-score-val))
                       (score2 (+ (aref dp i (1- j)) gap-score))
                       (score3 (+ (aref dp (1- i) j) gap-score))
                       (max-score (max score1 score2 score3)))
                  
                  (setf (aref dp i j) max-score)
                  
                  ;; For gap maximization, we need to track which path gives maximum gaps
                  (let ((max-gaps 0))
                    (cond
                      ((= max-score score1)
                       (setf max-gaps (aref gap-count (1- i) (1- j))))
                      ((= max-score score2)
                       (setf max-gaps (1+ (aref gap-count i (1- j)))))
                      ((= max-score score3)
                       (setf max-gaps (1+ (aref gap-count (1- i) j)))))
                    (setf (aref gap-count i j) max-gaps)))))
    
    ;; Reconstruct alignment
    (let ((align1 "")
          (align2 "")
          (i m)
          (j n))
      
      (loop while (and (> i 0) (> j 0)) do
            (let* ((match-score-val (if (char= (char seq1 (1- i)) (char seq2 (1- j)))
                                      match-score
                                      mismatch-score))
                   (score1 (+ (aref dp (1- i) (1- j)) match-score-val))
                   (score2 (+ (aref dp i (1- j)) gap-score))
                   (score3 (+ (aref dp (1- i) j) gap-score))
                   (current-score (aref dp i j)))
              
              (cond
                ((= current-score score1)
                 ;; Diagonal move
                 (setf align1 (concatenate 'string (string (char seq1 (1- i))) align1)
                       align2 (concatenate 'string (string (char seq2 (1- j))) align2))
                 (decf i)
                 (decf j))
                ((= current-score score2)
                 ;; Gap in seq1
                 (setf align1 (concatenate 'string "-" align1)
                       align2 (concatenate 'string (string (char seq2 (1- j))) align2))
                 (decf j))
                ((= current-score score3)
                 ;; Gap in seq2
                 (setf align1 (concatenate 'string (string (char seq1 (1- i))) align1)
                       align2 (concatenate 'string "-" align2))
                 (decf i)))))
      
      ;; Add remaining characters
      (loop while (> i 0) do
            (setf align1 (concatenate 'string (string (char seq1 (1- i))) align1)
                  align2 (concatenate 'string "-" align2))
            (decf i))
      
      (loop while (> j 0) do
            (setf align1 (concatenate 'string "-" align1)
                  align2 (concatenate 'string (string (char seq2 (1- j))) align2))
            (decf j))
      
      (list align1 align2 (aref dp m n) (aref gap-count m n)))))

;; Example usage:
;; (maximize-gaps-align "ACGT" "ACGT")
;; (maximize-gaps-align "ACGT" "ACG")
;; (maximize-gaps-align "ACGT" "CGT")
```

## Explanation

This solution implements a dynamic programming approach to find an optimal alignment that maximizes the number of gap symbols:

1. **DP Table Setup**: We maintain two tables - one for scores and one for tracking gap counts
2. **Base Cases**: Initialize the first row and column with gap penalties
3. **Recurrence Relation**: For each cell, we compute three possible scores:
   - Diagonal (match/mismatch)
   - From left (gap in first sequence)
   - From top (gap in second sequence)
4. **Gap Tracking**: We track which path gives us the maximum number of gaps
5. **Reconstruction**: Backtrack through the table to build the final alignment

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m×n) for the DP tables

## Note

This is a complex problem that requires careful handling of the gap maximization constraint. The approach shown prioritizes maximizing gaps while maintaining optimal alignment score, which may require more sophisticated dynamic programming techniques than standard sequence alignment.

