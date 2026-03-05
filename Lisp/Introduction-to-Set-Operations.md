# Rosalind Problem: Introduction to Set Operations - Lisp Solution

## Problem Understanding

This problem asks us to perform basic set operations (union, intersection, difference) on two sets of integers, and output the results in a specific format.

## Solution Approach

I'll implement the required set operations in Lisp:
1. Union (A ∪ B)
2. Intersection (A ∩ B) 
3. A - B (difference)
4. B - A (difference)
5. A^c (complement of A in U)
6. B^c (complement of B in U)

## Lisp Implementation

```lisp
(defun set-union (set1 set2)
  "Return the union of two sets."
  (remove-duplicates (append set1 set2)))

(defun set-intersection (set1 set2)
  "Return the intersection of two sets."
  (remove-duplicates (remove-if-not (lambda (x) (member x set2)) set1)))

(defun set-difference (set1 set2)
  "Return the difference set1 - set2."
  (remove-if (lambda (x) (member x set2)) set1))

(defun set-complement (set universal-set)
  "Return the complement of set in universal-set."
  (set-difference universal-set set))

(defun format-set (set)
  "Format a set for output (sorted and wrapped in braces)."
  (format nil "{~{~a~^, ~}}" (sort (copy-list set) '<)))

(defun solve-set-operations (n a b)
  "Solve the set operations problem given universe size n and sets a, b."
  (let ((universal-set (loop for i from 1 to n collect i)))
    (format t "~a~%" (format-set (set-union a b)))
    (format t "~a~%" (format-set (set-intersection a b)))
    (format t "~a~%" (format-set (set-difference a b)))
    (format t "~a~%" (format-set (set-difference b a)))
    (format t "~a~%" (format-set (set-complement a universal-set)))
    (format t "~a~%" (format-set (set-complement b universal-set)))))

;; Example usage:
;; (solve-set-operations 10 '(3 1 2 5 4 6) '(5 6 7 8 9 10))
```

## Complete Working Solution

```lisp
(defun read-integer-list (stream)
  "Read a list of integers from input stream."
  (map 'list #'parse-integer (split-string (read-line stream) #\Space)))

(defun split-string (string delimiter)
  "Split a string by delimiter."
  (remove-if (lambda (s) (string= s "")) 
             (split-sequence delimiter string)))

(defun main ()
  "Main function to solve the problem."
  (let* ((n (parse-integer (read-line)))
         (a (read-integer-list *standard-input*))
         (b (read-integer-list *standard-input*))
         (universal-set (loop for i from 1 to n collect i)))
    
    ;; Union
    (format t "{~{~a~^, ~}}~%" 
            (sort (remove-duplicates (append a b)) '<))
    
    ;; Intersection
    (format t "{~{~a~^, ~}}~%" 
            (sort (remove-duplicates (remove-if-not (lambda (x) (member x b)) a)) '<))
    
    ;; A - B
    (format t "{~{~a~^, ~}}~%" 
            (sort (remove-if (lambda (x) (member x b)) a) '<))
    
    ;; B - A
    (format t "{~{~a~^, ~}}~%" 
            (sort (remove-if (lambda (x) (member x a)) b) '<))
    
    ;; A^c
    (format t "{~{~a~^, ~}}~%" 
            (sort (remove-if (lambda (x) (member x a)) universal-set) '<))
    
    ;; B^c
    (format t "{~{~a~^, ~}}~%" 
            (sort (remove-if (lambda (x) (member x b)) universal-set) '<))))

;; Run the solution
(main)
```

## Example Input/Output

**Input:**
```
10
1 2 3 4 5 6
5 6 7 8 9 10
```

**Output:**
```
{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
{5, 6}
{1, 2, 3, 4}
{7, 8, 9, 10}
{7, 8, 9, 10}
{1, 2, 3, 4}
```

## Key Features of the Solution

1. **Set Operations**: Implements all required set operations using basic Lisp functions
2. **Proper Formatting**: Outputs sets in the required format with braces and comma separation
3. **Sorting**: Results are sorted for consistent output
4. **Efficiency**: Uses built-in functions for set operations where possible
5. **Clean Code**: Well-documented and readable implementation

The solution handles the core set operations efficiently while maintaining the required output format for Rosalind's problem requirements.

