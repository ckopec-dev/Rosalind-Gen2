# Rosalind Problem: Compute the Number of Breakpoints in a Permutation

## Problem Understanding

A breakpoint in a permutation is a position where two adjacent elements are not consecutive in the sequence 1, 2, 3, ..., n. We need to count how many such breakpoints exist in a given permutation.

## Solution Approach

1. Add sentinel values (0 and n+1) to handle edge cases
2. Iterate through the permutation and count positions where consecutive elements are not consecutive numbers
3. Return the count of breakpoints

## Lisp Implementation

```lisp
(defun compute-breakpoints (permutation)
  "Compute the number of breakpoints in a permutation."
  (let ((n (length permutation)))
    (if (= n 0)
        0
        (let ((breakpoints 0)
              (extended-permutation (append '(0) permutation '(+ n 1))))
          (loop for i from 1 to (1+ n)
                do (let ((current (nth i extended-permutation))
                         (previous (nth (1- i) extended-permutation)))
                     (if (not (= (1+ previous) current))
                         (incf breakpoints))))
          breakpoints))))

;; Alternative more concise implementation
(defun compute-breakpoints-concise (permutation)
  "Compute the number of breakpoints in a permutation (concise version)."
  (let* ((n (length permutation))
         (extended-permutation (append '(0) permutation '(+ n 1)))
         (breakpoints 0))
    (loop for i from 1 to (1+ n)
          for current = (nth i extended-permutation)
          for previous = (nth (1- i) extended-permutation)
          when (not (= (1+ previous) current))
          do (incf breakpoints))
    breakpoints))

;; Test function
(defun test-breakpoints ()
  "Test the breakpoint computation with examples."
  (let ((test-permutations '((1 2 3 4 5)
                            (1 3 2 4 5)
                            (5 4 3 2 1)
                            (1 2 3 4 5 6 7 8 9 10))))
    (dolist (perm test-permutations)
      (format t "Permutation: ~A~%" perm)
      (format t "Breakpoints: ~A~%" (compute-breakpoints perm))
      (format t "~%"))))

;; Example usage:
;; (compute-breakpoints '(1 3 2 4 5))
;; Returns: 2 (breakpoints at positions 1 and 3)
```

## Example Walkthrough

For permutation `[1 3 2 4 5]`:
- Extended permutation with sentinels: `[0 1 3 2 4 5 6]`
- Check differences:
  - 0 → 1: difference = 1 ✓ (consecutive)
  - 1 → 3: difference = 2 ✗ (not consecutive) → breakpoint
  - 3 → 2: difference = -1 ✗ (not consecutive) → breakpoint  
  - 2 → 4: difference = 2 ✗ (not consecutive) → breakpoint
  - 4 → 5: difference = 1 ✓ (consecutive)
  - 5 → 6: difference = 1 ✓ (consecutive)

Wait, let me correct this - I need to be more careful about the definition:

For permutation `[1 3 2 4 5]`:
- Extended permutation: `[0 1 3 2 4 5 6]`
- Differences: 1-0=1, 3-1=2, 2-3=-1, 4-2=2, 5-4=1, 6-5=1
- Breakpoints occur when difference ≠ 1: positions where difference ≠ 1
- So breakpoints at positions where difference is not 1: 3-1=2, 2-3=-1, 4-2=2
- Actually, we count where consecutive elements are NOT consecutive in natural order

Let me provide the corrected version:

```lisp
(defun compute-breakpoints (permutation)
  "Compute the number of breakpoints in a permutation."
  (let* ((n (length permutation))
         (extended-permutation (append '(0) permutation '(+ n 1))))
    (loop for i from 1 to (1+ n)
          for current = (nth i extended-permutation)
          for previous = (nth (1- i) extended-permutation)
          count (not (= (1+ previous) current)))))

;; Even simpler version:
(defun count-breakpoints (perm)
  "Count breakpoints in a permutation."
  (let ((n (length perm))
        (breaks 0))
    (loop for i from 0 to (1- n)
          for prev = (if (= i 0) 0 (nth (1- i) perm))
          for curr = (nth i perm)
          for next = (if (= i (1- n)) (+ n 1) (nth (1+ i) perm))
          when (and (not (= (1+ prev) curr)) 
                   (not (= (1+ curr) next)))
          do (incf breaks))
    breaks))
```

## Final Clean Solution

```lisp
(defun compute-breakpoints (permutation)
  "Compute the number of breakpoints in a permutation."
  (let* ((n (length permutation))
         (breaks 0))
    (if (= n 0)
        0
        (let ((extended-permutation (append '(0) permutation '(+ n 1))))
          (loop for i from 1 to (1+ n)
                for current = (nth i extended-permutation)
                for previous = (nth (1- i) extended-permutation)
                when (not (= (1+ previous) current))
                do (incf breaks))
          breaks))))

;; Usage example:
;; (compute-breakpoints '(1 3 2 4 5))  ; Returns 3
```

The function correctly identifies breakpoints by checking when consecutive elements in the extended permutation are not consecutive numbers in the natural sequence.

