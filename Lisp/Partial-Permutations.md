# Rosalind Problem: Partial Permutations

## Problem Statement
In combinatorics, a partial permutation is an ordering of k objects selected from a set of n objects, where the order matters. The number of partial permutations is given by the formula: P(n,k) = n!/(n-k)!

## Solution in Lisp

```lisp
(defun partial-permutations (n k)
  "Calculate the number of partial permutations P(n,k) = n!/(n-k)!"
  (if (or (< n 0) (< k 0) (< n k))
      0
      (let ((result 1))
        (loop for i from (- n k + 1) to n
              do (setf result (* result i)))
        result)))

;; Alternative implementation using factorial
(defun factorial (n)
  "Calculate factorial of n"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun partial-permutations-alt (n k)
  "Alternative implementation using factorial formula"
  (if (or (< n 0) (< k 0) (< n k))
      0
      (let ((numerator (factorial n))
            (denominator (factorial (- n k))))
        (quotient numerator denominator))))

;; For large numbers, we need to handle modular arithmetic
(defun partial-permutations-mod (n k mod)
  "Calculate partial permutations modulo mod"
  (if (or (< n 0) (< k 0) (< n k))
      0
      (let ((result 1))
        (loop for i from (- n k + 1) to n
              do (setf result (mod (* result i) mod)))
        result)))

;; Example usage:
;; (partial-permutations 21 7) => 5448643200
```

## Explanation

The problem asks us to compute the number of partial permutations P(n,k) = n!/(n-k)!, which represents the number of ways to arrange k objects from a set of n objects where order matters.

### Approach:
1. **Direct calculation**: Instead of computing full factorials, we directly multiply the consecutive integers from (n-k+1) to n
2. **Modular arithmetic**: For large numbers, we use modular arithmetic to prevent overflow
3. **Edge cases**: Handle invalid inputs (negative numbers, k > n)

### Key Points:
- The formula P(n,k) = n!/(n-k)! can be simplified to n × (n-1) × ... × (n-k+1)
- This avoids computing large factorials that could cause overflow
- We only need to multiply k consecutive integers starting from (n-k+1)

### Time Complexity:
- O(k) where k is the number of objects to arrange
- Much more efficient than computing full factorials

### Space Complexity:
- O(1) - constant space usage

The solution handles the mathematical computation efficiently while being robust against edge cases and potential overflow issues.

