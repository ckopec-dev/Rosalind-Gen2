# Rosalind Problem: Counting Quartets in Lisp

## Problem Understanding

The counting quartets problem asks us to count the number of quartets (4-tuples) that can be formed from a set of n elements, where the quartets are considered equivalent under certain conditions.

However, looking at the standard Rosalind problem "Counting Quartets", this typically refers to counting the number of ways to partition a set of n labeled elements into quartets (sets of 4 elements each), or more specifically, counting the number of ways to choose 4 elements from n elements.

But let me provide a more comprehensive solution that handles the typical interpretation of this problem.

## Solution Approach

The problem is asking for the number of ways to choose 4 elements from n elements, which is the combination formula: C(n,4) = n!/(4!(n-4)!)

## Lisp Implementation

```lisp
(defun factorial (n)
  "Calculate factorial of n"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun combination (n k)
  "Calculate combination C(n,k) = n!/(k!(n-k)!)"
  (if (or (< n 0) (< k 0) (> k n))
      0
      (/ (factorial n)
         (* (factorial k) (factorial (- n k))))))

(defun counting-quartets (n)
  "Count the number of ways to choose 4 elements from n elements"
  (combination n 4))

;; Alternative implementation using iterative approach for better efficiency
(defun combination-iter (n k)
  "Calculate combination C(n,k) iteratively to avoid large factorials"
  (if (or (< n 0) (< k 0) (> k n))
      0
      (let ((k (min k (- n k)))) ; Take smaller of k and n-k
        (if (= k 0)
            1
            (let ((result 1))
              (loop for i from 1 to k
                    do (setf result (/ (* result (- n i -1)) i)))
              result)))))

(defun counting-quartets-optimized (n)
  "Optimized version to count quartets"
  (combination-iter n 4))

;; Test function
(defun test-counting-quartets ()
  "Test the counting quartets function"
  (format t "Number of quartets from 4 elements: ~A~%" (counting-quartets-optimized 4))
  (format t "Number of quartets from 5 elements: ~A~%" (counting-quartets-optimized 5))
  (format t "Number of quartets from 6 elements: ~A~%" (counting-quartets-optimized 6))
  (format t "Number of quartets from 10 elements: ~A~%" (counting-quartets-optimized 10)))
```

## Example Usage

```lisp
;; Example usage:
(counting-quartets-optimized 4)  ; Returns 1
(counting-quartets-optimized 5)  ; Returns 5
(counting-quartets-optimized 6)  ; Returns 15
(counting-quartets-optimized 10) ; Returns 210
```

## Explanation

1. **Factorial Function**: Calculates n! for use in combination formula
2. **Combination Function**: Computes C(n,k) = n!/(k!(n-k)!)
3. **Optimized Combination**: Uses iterative approach to avoid computing large factorials directly
4. **Counting Quartets**: Special case where k=4, so we compute C(n,4)

## Mathematical Formula

The number of ways to choose 4 elements from n elements is:
$$C(n,4) = \frac{n!}{4!(n-4)!} = \frac{n(n-1)(n-2)(n-3)}{24}$$

## Sample Input/Output

For input n = 6:
- Output: 15
- This represents the 15 ways to choose 4 elements from a set of 6 elements

The optimized version is preferred for larger values of n as it avoids computing large factorials and is more numerically stable.

