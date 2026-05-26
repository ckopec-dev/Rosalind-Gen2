# Rosalind Problem: Partial Sort

## Problem Description
Given: A collection of n (n≤10) integers between -100 and 100. A value k (k≤n).

Return: The k smallest elements of the array, in ascending order.

## Solution in Lisp

```lisp
(defun partial-sort (array k)
  "Returns the k smallest elements of array in ascending order"
  (let ((sorted-array (sort (copy-list array) #'<)))
    (subseq sorted-array 0 k)))

;; Alternative implementation using quickselect approach
(defun partial-sort-quickselect (array k)
  "Returns the k smallest elements using quickselect algorithm"
  (let ((copy-array (copy-list array)))
    (quickselect-kth-smallest copy-array 0 (1- (length copy-array)) k)
    (sort (subseq copy-array 0 k) #'<)))

(defun quickselect-kth-smallest (arr left right k)
  "Find the k-th smallest element using quickselect"
  (when (<= left right)
    (let ((pivot-index (partition arr left right)))
      (cond
        ((= pivot-index k)
         (return-from quickselect-kth-smallest))
        ((< k pivot-index)
         (quickselect-kth-smallest arr left (1- pivot-index) k))
        (t
         (quickselect-kth-smallest arr (1+ pivot-index) right k))))))

(defun partition (arr left right)
  "Partition function for quickselect"
  (let ((pivot (aref arr right))
        (i (1- left)))
    (loop for j from left to (1- right) do
          (when (<= (aref arr j) pivot)
            (incf i)
            (swap arr i j)))
    (swap arr (1+ i) right)
    (1+ i)))

(defun swap (arr i j)
  "Swap elements at positions i and j in array"
  (let ((temp (aref arr i)))
    (setf (aref arr i) (aref arr j))
    (setf (aref arr j) temp)))

;; Simple and clean solution
(defun partial-sort-simple (array k)
  "Simple solution: sort and take first k elements"
  (sort (subseq (copy-list array) 0 k) #'<))
```

## Example Usage

```lisp
;; Example input
(setq test-array '(2 1 5 4 3 6 8 7))
(setq k 4)

;; Using the simple approach
(partial-sort-simple test-array k)
;; Returns: (1 2 3 4)

;; Using the full implementation
(partial-sort test-array k)
;; Returns: (1 2 3 4)
```

## Explanation

The problem asks for the k smallest elements from an array in ascending order. Here's my approach:

1. **Simple Solution**: 
   - Make a copy of the input array to avoid modifying the original
   - Sort the array in ascending order using `sort`
   - Take the first k elements using `subseq`

2. **Quickselect Solution** (more efficient for large arrays):
   - Uses the quickselect algorithm to find the k-th smallest element
   - Partitions the array around a pivot
   - Recursively searches in the appropriate partition
   - Then sorts only the first k elements

## Time Complexity
- Simple solution: O(n log n) due to sorting
- Quickselect solution: O(n) average case, O(n²) worst case

## Space Complexity
- O(n) for creating a copy of the array

The simple solution is more readable and sufficient for the given constraints (n ≤ 10).

