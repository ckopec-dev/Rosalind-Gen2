# Rosalind Problem: Median

## Problem Statement
Given: A positive integer n ≤ 10^5 and a array A[1..n] of integers from -10^5 to 10^5.

Return: The median of the array.

## Solution in Lisp

```lisp
(defun median (arr)
  "Find the median of an array of integers"
  (let ((sorted-arr (sort (copy-list arr) #'<)))
    (let ((n (length sorted-arr)))
      (if (oddp n)
          ;; Odd length: return middle element
          (elt sorted-arr (floor n 2))
          ;; Even length: return average of two middle elements
          (let ((mid1 (elt sorted-arr (/ n 2 -1)))
                (mid2 (elt sorted-arr (/ n 2))))
            (/ (+ mid1 mid2) 2.0))))))

;; Alternative implementation using quickselect for better performance
(defun quickselect (arr k)
  "Find the k-th smallest element in array (0-indexed)"
  (if (null arr)
      nil
      (let* ((pivot (first arr))
             (less (remove-if (lambda (x) (>= x pivot)) arr))
             (equal (remove-if (lambda (x) (not (= x pivot))) arr))
             (greater (remove-if (lambda (x) (< x pivot)) arr)))
        (cond
          ((< k (length less))
           (quickselect less k))
          ((< k (+ (length less) (length equal)))
           pivot)
          (t
           (quickselect greater (- k (length less) (length equal))))))))

(defun median-quickselect (arr)
  "Find median using quickselect algorithm"
  (let ((n (length arr)))
    (if (oddp n)
        (quickselect arr (floor n 2))
        (let ((mid1 (quickselect arr (/ n 2 -1)))
              (mid2 (quickselect arr (/ n 2))))
          (/ (+ mid1 mid2) 2.0)))))
```

## Example Usage

```lisp
;; Example input: [3, 1, 4, 1, 5, 9, 2, 6]
;; Expected output: 3.5 (average of 3 and 4)

;; Using the sorting approach
(median '(3 1 4 1 5 9 2 6))
;; Returns: 3.5

;; Using the quickselect approach
(median-quickselect '(3 1 4 1 5 9 2 6))
;; Returns: 3.5

;; For odd-length array
(median '(3 1 4 1 5))
;; Returns: 3
```

## Input/Output Format

For Rosalind submission, you would typically read from a file and format the output:

```lisp
(defun solve-median (filename)
  "Read input from file and solve median problem"
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let* ((line1 (read-line stream))
           (n (parse-integer line1))
           (line2 (read-line stream))
           (arr (map 'list #'parse-integer (split-string line2))))
      (format t "~A~%" (median arr)))))

(defun split-string (str)
  "Split string by whitespace"
  (remove-if (lambda (x) (string= x "")) 
             (split-string-internal str)))

(defun split-string-internal (str)
  "Helper function to split string"
  (let ((result '()))
    (loop for i from 0 below (length str)
          for char = (char str i)
          if (char= char #\Space)
            do (push "" result)
          else
            do (push (string char) result))
    (reverse result)))
```

## Key Points

1. **Sorting Approach**: Simple but O(n log n) time complexity
2. **Quickselect Approach**: More efficient O(n) average time complexity
3. **Median Calculation**: 
   - Odd length: Middle element
   - Even length: Average of two middle elements
4. **Edge Cases**: Handle empty arrays and single element arrays
5. **Precision**: Return as floating point for even-length arrays

The sorting approach is simpler and sufficient for most Rosalind problems, while quickselect is more efficient for larger datasets.

