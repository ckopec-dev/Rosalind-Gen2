# Rosalind Problem: Merge_Two_Sorted_Arrays in Lisp

## Problem Description
Given two sorted arrays, merge them into a single sorted array.

## Solution

```lisp
(defun merge-two-sorted-arrays (array1 array2)
  "Merge two sorted arrays into one sorted array."
  (let ((result '())
        (i 0)
        (j 0))
    ;; While both arrays have elements left
    (loop while (and (< i (length array1))
                     (< j (length array2)))
          do (if (<= (aref array1 i) (aref array2 j))
                 (progn
                   (push (aref array1 i) result)
                   (incf i))
                 (progn
                   (push (aref array2 j) result)
                   (incf j)))))
    
    ;; Add remaining elements from array1
    (loop while (< i (length array1))
          do (progn
               (push (aref array1 i) result)
               (incf i)))
    
    ;; Add remaining elements from array2
    (loop while (< j (length array2))
          do (progn
               (push (aref array2 j) result)
               (incf j)))
    
    ;; Reverse result since we built it backwards
    (nreverse result)))

;; Alternative more concise version using recursion
(defun merge-sorted-arrays (arr1 arr2)
  "Recursively merge two sorted arrays."
  (cond
    ;; If first array is empty, return second array
    ((null arr1) arr2)
    ;; If second array is empty, return first array
    ((null arr2) arr1)
    ;; If first element of arr1 is smaller or equal
    ((<= (first arr1) (first arr2))
     (cons (first arr1) (merge-sorted-arrays (rest arr1) arr2)))
    ;; Otherwise, first element of arr2 is smaller
    (t
     (cons (first arr2) (merge-sorted-arrays arr1 (rest arr2))))))

;; For array-based approach (if inputs are arrays)
(defun merge-arrays (arr1 arr2)
  "Merge two sorted arrays using array indexing."
  (let ((len1 (length arr1))
        (len2 (length arr2))
        (result (make-array (+ len1 len2) :element-type 'integer))
        (i 0)
        (j 0)
        (k 0))
    ;; Merge while both arrays have elements
    (loop while (and (< i len1) (< j len2))
          do (if (<= (aref arr1 i) (aref arr2 j))
                 (progn
                   (setf (aref result k) (aref arr1 i))
                   (incf i)
                   (incf k))
                 (progn
                   (setf (aref result k) (aref arr2 j))
                   (incf j)
                   (incf k)))))
    
    ;; Copy remaining elements from arr1
    (loop while (< i len1)
          do (progn
               (setf (aref result k) (aref arr1 i))
               (incf i)
               (incf k)))
    
    ;; Copy remaining elements from arr2
    (loop while (< j len2)
          do (progn
               (setf (aref result k) (aref arr2 j))
               (incf j)
               (incf k)))
    
    result))
```

## Example Usage

```lisp
;; Test with lists
(merge-sorted-arrays '(1 3 5 7) '(2 4 6 8))
;; Returns: (1 2 3 4 5 6 7 8)

;; Test with arrays
(let ((arr1 (vector 1 3 5 7))
      (arr2 (vector 2 4 6 8)))
  (merge-arrays arr1 arr2))
;; Returns: #*1 2 3 4 5 6 7 8
```

## Explanation

The solution provides three approaches:

1. **Iterative with lists**: Uses a loop to compare elements and build the result
2. **Recursive approach**: Uses Lisp's built-in list operations for a clean recursive solution
3. **Array-based approach**: Uses array indexing for better performance with large datasets

The key insight is to compare the current elements of both arrays and always take the smaller one, continuing until one array is exhausted, then appending the remaining elements from the other array.

Time complexity: O(n + m) where n and m are the lengths of the arrays
Space complexity: O(n + m) for the merged result

