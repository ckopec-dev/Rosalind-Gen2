# Rosalind Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

## Problem Description
Given a cyclic peptide, we need to generate its theoretical spectrum, which consists of all subpeptide masses that can be formed by cutting the cyclic peptide at different positions.

## Solution in Lisp

```lisp
(defun generate-cyclic-spectrum (peptide)
  "Generate the theoretical spectrum of a cyclic peptide.
   Returns a list of all subpeptide masses sorted in ascending order."
  (let ((mass-table (make-mass-table))
        (n (length peptide))
        (spectrum '()))
    
    ;; Generate all possible subpeptides
    (loop for i from 0 below n do
      (loop for j from 1 to n do
        (let ((subpeptide (get-subpeptide peptide i j n)))
          (push (calculate-mass subpeptide mass-table) spectrum))))
    
    ;; Add the mass of the entire peptide
    (push (calculate-mass peptide mass-table) spectrum)
    
    ;; Add 0 for the empty subpeptide
    (push 0 spectrum)
    
    ;; Sort and return
    (sort spectrum '<)))

(defun make-mass-table ()
  "Create a hash table mapping amino acid letters to their masses."
  (let ((mass-table (make-hash-table :test 'equal)))
    (setf (gethash "A" mass-table) 71)
    (setf (gethash "C" mass-table) 103)
    (setf (gethash "D" mass-table) 115)
    (setf (gethash "E" mass-table) 129)
    (setf (gethash "F" mass-table) 147)
    (setf (gethash "G" mass-table) 57)
    (setf (gethash "H" mass-table) 137)
    (setf (gethash "I" mass-table) 113)
    (setf (gethash "K" mass-table) 128)
    (setf (gethash "L" mass-table) 113)
    (setf (gethash "M" mass-table) 131)
    (setf (gethash "N" mass-table) 114)
    (setf (gethash "P" mass-table) 97)
    (setf (gethash "Q" mass-table) 128)
    (setf (gethash "R" mass-table) 156)
    (setf (gethash "S" mass-table) 87)
    (setf (gethash "T" mass-table) 101)
    (setf (gethash "V" mass-table) 99)
    (setf (gethash "W" mass-table) 186)
    (setf (gethash "Y" mass-table) 163)
    (setf (gethash "X" mass-table) 0)  ; Unknown amino acid
    (setf (gethash "Z" mass-table) 0)  ; Unknown amino acid
    mass-table))

(defun get-subpeptide (peptide start length total-length)
  "Get a subpeptide from a cyclic peptide.
   start: starting position (0-indexed)
   length: length of subpeptide
   total-length: total length of peptide"
  (let ((result '()))
    (loop for i from 0 below length do
      (let ((position (mod (+ start i) total-length)))
        (push (char peptide position) result))))
    (coerce (nreverse result) 'string)))

(defun calculate-mass (peptide mass-table)
  "Calculate the total mass of a peptide."
  (let ((total-mass 0))
    (loop for i from 0 below (length peptide) do
      (let ((aa (string (char peptide i))))
        (incf total-mass (gethash aa mass-table))))
    total-mass))

;; Example usage:
;; (generate-cyclic-spectrum "LEQN")
```

## Explanation

The solution works as follows:

1. **Mass Table**: First, we create a hash table mapping each amino acid to its mass according to the standard amino acid mass table.

2. **Subpeptide Generation**: For a cyclic peptide of length n, we generate all possible subpeptides by:
   - Starting at each position (0 to n-1)
   - Taking subpeptides of all possible lengths (1 to n)
   - Using modular arithmetic to handle the cyclic nature

3. **Mass Calculation**: For each subpeptide, we calculate its total mass by summing up the masses of individual amino acids.

4. **Spectrum Construction**: We include:
   - The mass of each subpeptide
   - The mass of the entire peptide (full cycle)
   - The mass of the empty subpeptide (0)

5. **Sorting**: Finally, we sort the spectrum in ascending order.

## Example

For the peptide "LEQN":
- Subpeptides include: L, E, Q, N, LE, EQ, QN, NE, LEQ, EQN, QNL, NLE, LEQN
- Each subpeptide's mass is calculated
- The result is sorted ascending order

## Time Complexity
- O(n³) where n is the length of the peptide
- O(n²) subpeptides, each taking O(n) time to calculate mass

## Space Complexity
- O(n²) for storing all subpeptide masses

