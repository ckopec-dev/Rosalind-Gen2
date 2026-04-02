# Rosalind Problem: Convert_a_Peptide_Vector_into_a_Peptide (Lisp Solution)

## Problem Understanding

Given a peptide vector (a binary vector representing the masses of amino acids in a peptide), we need to convert it back into the actual peptide sequence.

The peptide vector has length 3n where n is the length of the peptide, and each position represents:
- Position 3i: mass 113 (A)
- Position 3i+1: mass 114 (R) 
- Position 3i+2: mass 128 (N)

## Solution

```lisp
(defun convert-peptide-vector-to-peptide (vector)
  "Convert a peptide vector into a peptide string.
   Each amino acid is represented by 3 positions in the vector.
   113 -> A, 114 -> R, 128 -> N"
  (let ((amino-acids '((113 . "A") (114 . "R") (128 . "N")))
        (result '()))
    ;; Process the vector in groups of 3
    (loop for i from 0 below (length vector) by 3
          for mass1 = (aref vector (+ i 0))
          for mass2 = (aref vector (+ i 1))
          for mass3 = (aref vector (+ i 2))
          for amino1 = (cdr (assoc mass1 amino-acids))
          for amino2 = (cdr (assoc mass2 amino-acids))
          for amino3 = (cdr (assoc mass3 amino-acids))
          do (progn
               (when amino1 (push amino1 result))
               (when amino2 (push amino2 result))
               (when amino3 (push amino3 result)))))
    ;; Reverse the result since we pushed in reverse order
    (apply #'concatenate 'string (reverse result))))

;; Alternative cleaner approach
(defun convert-peptide-vector-to-peptide-v2 (vector)
  "Cleaner version: Convert peptide vector to peptide string"
  (let ((mass-to-amino '((113 . "A") (114 . "R") (128 . "N")))
        (peptide '()))
    (loop for i from 0 below (length vector) by 3
          for mass = (aref vector i)
          for amino = (cdr (assoc mass mass-to-amino))
          do (push amino peptide))
    (apply #'concatenate 'string (reverse peptide))))

;; Even more concise version
(defun convert-peptide-vector-to-peptide-v3 (vector)
  "Most concise version"
  (let ((mass-to-amino '((113 . "A") (114 . "R") (128 . "N"))))
    (let ((peptide '()))
      (loop for i from 0 below (length vector) by 3
            for amino = (cdr (assoc (aref vector i) mass-to-amino))
            do (push amino peptide))
      (apply #'concatenate 'string (reverse peptide)))))
```

## Example Usage

```lisp
;; Example vector (representing peptide "ARN")
(setq example-vector (make-array 6 
                                 :element-type 'fixnum 
                                 :initial-contents '(113 114 128 113 114 128)))

;; Convert to peptide
(convert-peptide-vector-to-peptide-v3 example-vector)
;; Returns: "ARN"
```

## Explanation

1. **Input**: A peptide vector represented as an array of integers
2. **Processing**: 
   - Process the vector in groups of 3 elements
   - Each group corresponds to a single amino acid
   - Use the first element of each group to determine which amino acid it represents
3. **Mapping**: 
   - 113 → "A" (Alanine)
   - 114 → "R" (Arginine) 
   - 128 → "N" (Asparagine)
4. **Output**: The peptide string constructed from the amino acids

The key insight is that the peptide vector encodes amino acids in groups of 3 positions, where the first position of each group determines which amino acid to use.

