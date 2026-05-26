# Rosalind Problem: Find a Position in a Genome Minimizing the Skew

## Problem Understanding

The skew of a genome is defined as the difference between the number of occurrences of 'G' and the number of occurrences of 'C' in the genome. We need to find the position(s) where the skew reaches its minimum value.

## Solution Approach

1. Calculate the skew at each position in the genome
2. Track the minimum skew value and its positions
3. Return the position(s) where minimum skew occurs

## Lisp Implementation

```lisp
(defun find-min-skew-position (genome)
  "Find the position(s) in genome where skew reaches minimum value.
   Skew = # of G - # of C at each position."
  (let ((min-skew most-positive-fixnum)
        (current-skew 0)
        (positions '()))
    (loop for i from 0 below (length genome)
          for nucleotide = (char genome i)
          do (progn
               ;; Update skew based on current nucleotide
               (cond ((char= nucleotide #\G) (incf current-skew))
                     ((char= nucleotide #\C) (decf current-skew)))
               
               ;; Check if we found a new minimum
               (cond ((< current-skew min-skew)
                      (setf min-skew current-skew
                            positions (list i)))
                     ((= current-skew min-skew)
                      (push i positions)))))
    
    ;; Return positions in ascending order
    (nreverse positions)))

;; Alternative implementation that's more explicit
(defun find-min-skew-position-alt (genome)
  "Alternative implementation with clearer step-by-step calculation."
  (let ((skew-values (make-array (1+ (length genome)) :initial-element 0))
        (min-skew most-positive-fixnum)
        (positions '()))
    
    ;; Calculate skew at each position
    (loop for i from 1 to (length genome)
          for nucleotide = (char genome (1- i))
          do (progn
               (setf (aref skew-values i) 
                     (if (char= nucleotide #\G)
                         (1+ (aref skew-values (1- i)))
                         (if (char= nucleotide #\C)
                             (1- (aref skew-values (1- i)))
                             (aref skew-values (1- i)))))
               
               ;; Track minimum skew
               (let ((current-skew (aref skew-values i)))
                 (cond ((< current-skew min-skew)
                        (setf min-skew current-skew
                              positions (list (1- i))))
                       ((= current-skew min-skew)
                        (push (1- i) positions))))))
    
    ;; Return positions in ascending order
    (nreverse positions)))

;; Simple and clean version
(defun find-min-skew (genome)
  "Find positions where skew is minimum."
  (let ((min-skew 0)
        (current-skew 0)
        (positions '()))
    (loop for i from 0 below (length genome)
          for nucleotide = (char genome i)
          do (progn
               ;; Update skew
               (cond ((char= nucleotide #\G) (incf current-skew))
                     ((char= nucleotide #\C) (decf current-skew)))
               
               ;; Check for new minimum
               (cond ((< current-skew min-skew)
                      (setf min-skew current-skew
                            positions (list i)))
                     ((= current-skew min-skew)
                      (push i positions)))))
    
    ;; Return positions in ascending order
    (nreverse positions)))
```

## Example Usage

```lisp
;; Test with example genome
(setq example-genome "CCTATCGGTGGATTAGCATGTCCCTGTACGTTTCGCCGCGTTTTTTTTTTTCTTCTCCGGCGCGCCGGGACACGAGCGGCGGCAGCGCGGCGGCGCGCGCCGGGCGCGCGCCGGGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCG

