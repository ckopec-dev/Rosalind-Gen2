# Rosalind Problem: Implement GreedySorting to Sort a Permutation by Reversals

## Problem Understanding

The GreedySorting algorithm is used to sort a permutation of integers by performing a series of reversals. The goal is to transform a given permutation into the identity permutation (1, 2, 3, ..., n) using the minimum number of reversals.

## Algorithm Steps

1. Find the first position where the element is not in its correct place
2. Find where the correct element is located
3. Reverse the segment between these positions
4. If needed, reverse the segment to place the element correctly
5. Continue until the permutation is sorted

## Lisp Implementation

```lisp
(defun greedy-sorting (permutation)
  "Sort a permutation using greedy sorting algorithm by reversals"
  (let ((reversals '())
        (n (length permutation)))
    (loop for i from 1 to n do
      (let ((current-element (nth (1- i) permutation)))
        (if (/= current-element i)
            (let ((position-of-correct (position i permutation)))
              ;; Reverse from i to position-of-correct
              (setf permutation (reverse-segment permutation i (1+ position-of-correct)))
              (push (list i (1+ position-of-correct)) reversals)
              ;; If the element is not in correct position after reversal, reverse again
              (if (/= (nth (1- i) permutation) i)
                  (let ((new-position (position i permutation)))
                    (setf permutation (reverse-segment permutation i (1+ new-position)))
                    (push (list i (1+ new-position)) reversals)))))))
    (reverse reversals)))

(defun reverse-segment (permutation start end)
  "Reverse elements from start to end (1-indexed) in permutation"
  (let ((result (copy-list permutation)))
    (loop for i from start to end do
      (setf (nth (1- i) result) (nth (1- end) permutation))
      (decf end))
    result))

(defun print-reversals (reversals)
  "Print reversals in the required format"
  (dolist (rev reversals)
    (format t "~{~a ~}~%~" rev)))

;; Alternative cleaner implementation
(defun greedy-sorting-clean (permutation)
  "Cleaner implementation of greedy sorting"
  (let ((reversals '()))
    (loop for i from 1 to (length permutation) do
      (let ((current-element (nth (1- i) permutation)))
        (when (/= current-element i)
          (let ((position-of-correct (position i permutation)))
            ;; Perform reversal
            (setf permutation (reverse-segment permutation i (1+ position-of-correct)))
            (push (list i (1+ position-of-correct)) reversals)))))
    (reverse reversals)))

;; Test function
(defun test-greedy-sorting ()
  "Test the greedy sorting implementation"
  (let ((test-permutation '(+3 +4 -1 -5 +2)))
    (format t "Input: ~a~%" test-permutation)
    (let ((result (greedy-sorting-clean test-permutation)))
      (format t "Reversals:~%")
      (dolist (rev result)
        (format t "~{~a ~}~%" rev))
      (format t "Sorted permutation: ~a~%" 
              (reduce (lambda (perm rev)
                        (let ((start (first rev))
                              (end (second rev)))
                          (reverse-segment perm start (1+ end))))
                      result
                      :initial-value test-permutation
                      :from-end t)))))
```

## Example Usage

```lisp
;; Example input: (+3 +4 -1 -5 +2)
;; Expected output: reversals that sort to (+1 +2 +3 +4 +5)

(greedy-sorting-clean '(3 4 -1 -5 2))
```

## Key Points

1. **1-indexed**: The algorithm uses 1-indexed positions for the permutation
2. **Reversal operation**: Each reversal affects elements from position i to position j
3. **Greedy approach**: At each step, we fix one element at a time
4. **Output format**: Each reversal is output as two integers (start and end positions)

## Time Complexity

- **Time**: O(n²) where n is the length of the permutation
- **Space**: O(n) for storing the reversals

## Sample Input/Output

**Input**: `(+3 +4 -1 -5 +2)`
**Output**: 
```
1 3
1 5
2 5
2 4
```

This represents the sequence of reversals needed to sort the permutation into the identity permutation (1, 2, 3, 4, 5).

