# Rosalind Problem: Compute the Number of Peptides of Given Total Mass

## Problem Description
Given an integer mass, compute the number of different peptides that can have that total mass, where each amino acid has a specific mass.

## Solution in Lisp

```lisp
(defun compute-number-of-peptides (total-mass)
  "Compute the number of different peptides that can have the given total mass."
  (let ((mass-table '((57 . 1) (71 . 1) (87 . 1) (97 . 1) (99 . 1) 
                      (101 . 1) (103 . 1) (113 . 1) (114 . 1) (115 . 1)
                      (128 . 1) (129 . 1) (131 . 1) (137 . 1) (147 . 1)
                      (156 . 1) (163 . 1) (186 . 1))))
    (let ((dp (make-array (1+ total-mass) :element-type 'fixnum :initial-element 0)))
      (setf (aref dp 0) 1) ; Base case: one way to make mass 0 (empty peptide)
      
      ;; For each mass from 1 to total-mass
      (loop for i from 1 to total-mass do
            (loop for (mass . count) in mass-table do
                  (when (and (>= i mass) (> (aref dp (- i mass)) 0))
                    (incf (aref dp i) (aref dp (- i mass)))))
      
      (aref dp total-mass))))

;; Alternative implementation using dynamic programming with memoization
(defun compute-number-of-peptides-memo (total-mass &optional (memo nil))
  "Compute number of peptides using memoization approach."
  (if (<= total-mass 0)
      (if (= total-mass 0) 1 0)
      (let ((mass-table '(57 71 87 97 99 101 103 113 114 115 128 129 131 137 147 156 163 186)))
        (if (and memo (gethash total-mass memo))
            (gethash total-mass memo)
            (let ((result 0))
              (loop for mass in mass-table do
                    (when (>= total-mass mass)
                      (incf result (compute-number-of-peptides-memo (- total-mass mass) memo))))
              (if memo
                  (setf (gethash total-mass memo) result)
                  (let ((memo-table (make-hash-table)))
                    (setf (gethash total-mass memo-table) result)
                    (return-from compute-number-of-peptides-memo 
                      (compute-number-of-peptides-memo total-mass memo-table)))))
              result)))))

;; Most efficient iterative solution
(defun compute-number-of-peptides-fast (total-mass)
  "Fast iterative solution for computing number of peptides with given mass."
  (let ((masses '(57 71 87 97 99 101 103 113 114 115 128 129 131 137 147 156 163 186))
        (dp (make-array (1+ total-mass) :element-type 'fixnum :initial-element 0)))
    (setf (aref dp 0) 1) ; One way to make mass 0
    
    ;; Fill dp array using dynamic programming
    (loop for i from 1 to total-mass do
          (loop for mass in masses do
                (when (>= i mass)
                  (incf (aref dp i) (aref dp (- i mass))))))
    
    (aref dp total-mass)))

;; Example usage:
;; (compute-number-of-peptides-fast 1024)
```

## Explanation

This problem is solved using dynamic programming, similar to the coin change problem. The approach is:

1. **Base Case**: There's one way to make mass 0 (empty peptide)
2. **Recurrence Relation**: For each mass `i`, we sum up all ways to make `i - mass_of_amino_acid` for each valid amino acid
3. **Amino Acid Masses**: The 18 standard amino acid masses are used as denominations

## Time and Space Complexity
- **Time Complexity**: O(total_mass × number_of_amino_acids)
- **Space Complexity**: O(total_mass)

## Example
For mass 1024, the function will return the number of different peptide combinations that sum to exactly 1024 daltons.

The iterative approach (`compute-number-of-peptides-fast`) is recommended as it's most efficient and straightforward to understand.

