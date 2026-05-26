# Rosalind Problem: Rabbits and Recurrence Relations (Lisp Solution)

## Problem Statement
A sequence of numbers is called a recurrence relation if it is defined recursively. For the Fibonacci sequence, we have F(n) = F(n-1) + F(n-2) with base cases F(1) = 1 and F(2) = 1.

Given: Positive integers n ≤ 40 and k ≤ 5.

Return: The total number of rabbit pairs that will be present after n months if we begin with 1 pair and in each generation, every pair of reproduction-age rabbits produces a litter of k rabbit pairs (instead of only 1 pair).

## Solution Approach
This is a classic Fibonacci-like recurrence relation where:
- Each month, we have F(n) pairs
- Each pair produces k new pairs each month
- The recurrence relation is: F(n) = F(n-1) + k × F(n-2)

## Lisp Implementation

```lisp
(defun rabbits-and-recurrence (n k)
  "Calculate the number of rabbit pairs after n months with k pairs per litter"
  (cond
    ((= n 1) 1)
    ((= n 2) 1)
    (t (let ((prev2 1)  ; F(1) = 1
             (prev1 1))  ; F(2) = 1
         (loop for i from 3 to n
               for current = (+ prev1 (* k prev2)) then (+ prev1 (* k prev2))
               do (setf prev2 prev1
                        prev1 current))
         prev1))))

;; Alternative recursive implementation with memoization
(defun rabbits-rec-memo (n k &optional (memo (make-hash-table)))
  "Recursive implementation with memoization"
  (cond
    ((= n 1) 1)
    ((= n 2) 1)
    (t (let ((result (gethash (cons n k) memo)))
         (if result
             result
             (let ((value (+ (rabbits-rec-memo (- n 1) k memo)
                            (* k (rabbits-rec-memo (- n 2) k memo)))))
               (setf (gethash (cons n k) memo) value)
               value))))))

;; Simple iterative approach (most efficient)
(defun rabbits-iterative (n k)
  "Iterative solution for rabbits and recurrence relation"
  (if (<= n 2)
      1
      (let ((a 1)  ; F(n-2)
            (b 1))  ; F(n-1)
        (loop for i from 3 to n
              do (let ((temp b))
                   (setf b (+ b (* k a))
                         a temp)))
        b)))

;; Example usage:
;; (rabbits-and-recurrence 5 3)  ; Should return 19
;; (rabbits-iterative 5 3)      ; Should return 19
```

## Example Walkthrough
For n=5, k=3:
- Month 1: 1 pair
- Month 2: 1 pair  
- Month 3: 1 + 3×1 = 4 pairs
- Month 4: 4 + 3×1 = 7 pairs
- Month 5: 7 + 3×4 = 19 pairs

## Test Cases

```lisp
;; Test the function with given example
(rabbits-iterative 5 3)  ; Expected: 19

;; Additional test cases
(rabbits-iterative 1 1)  ; Expected: 1
(rabbits-iterative 2 1)  ; Expected: 1
(rabbits-iterative 3 1)  ; Expected: 2
(rabbits-iterative 4 2)  ; Expected: 5
```

## Complexity Analysis
- **Time Complexity**: O(n) - single loop through months
- **Space Complexity**: O(1) - only using constant extra space

The iterative solution is most efficient for this problem as it avoids the overhead of recursion and memoization while maintaining clear, readable code.

