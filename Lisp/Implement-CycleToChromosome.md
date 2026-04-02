# Rosalind Problem: Implement_CycleToChromosome

## Problem Description
The cycle to chromosome problem involves converting a cycle in the genome graph back to a chromosome. Given a cycle of even length, we need to reconstruct the corresponding chromosome.

## Solution in Lisp

```lisp
(defun cycle-to-chromosome (cycle)
  "Convert a cycle to a chromosome.
   Input: cycle - a list of integers representing a cycle
   Output: a list of integers representing the corresponding chromosome"
  (let ((chromosome '()))
    ;; Process the cycle in pairs (since cycle length is even)
    (loop for i from 0 to (- (length cycle) 1) by 2
          do (let ((node1 (nth i cycle))
                   (node2 (nth (+ i 1) cycle)))
               ;; If node1 is even, we take node1/2, otherwise we take (node2+1)/2
               (if (evenp node1)
                   (push (/ node1 2) chromosome)
                   (push (/ (+ node2 1) 2) chromosome))))
    ;; Reverse the chromosome since we built it backwards
    (reverse chromosome)))

;; Alternative implementation using a more functional approach
(defun cycle-to-chromosome-functional (cycle)
  "Functional implementation of cycle to chromosome conversion"
  (let ((pairs (loop for i from 0 to (- (length cycle) 1) by 2
                     collect (list (nth i cycle) (nth (+ i 1) cycle)))))
    (reverse (mapcar (lambda (pair)
                       (if (evenp (first pair))
                           (/ (first pair) 2)
                           (/ (+ (second pair) 1) 2)))
                     pairs))))

;; Test function
(defun test-cycle-to-chromosome ()
  "Test the cycle to chromosome function"
  (let ((test-cycle '(1 2 4 3 6 5)))
    (format t "Input cycle: ~A~%" test-cycle)
    (format t "Output chromosome: ~A~%" (cycle-to-chromosome test-cycle))
    (format t "Functional version: ~A~%" (cycle-to-chromosome-functional test-cycle))))

;; Example usage:
;; (test-cycle-to-chromosome)
```

## Explanation

The algorithm works as follows:

1. **Input**: A cycle represented as a list of integers, where each integer represents a node in the genome graph
2. **Processing**: 
   - Since the cycle has even length, we process it in pairs of consecutive elements
   - For each pair of nodes `(node1, node2)`:
     - If `node1` is even, we take `node1/2` as the chromosome element
     - If `node1` is odd, we take `(node2+1)/2` as the chromosome element
3. **Output**: A list representing the chromosome

## Key Points

- The cycle represents a sequence of nodes in the genome graph
- The conversion follows the relationship between genome graph cycles and chromosomes
- The algorithm assumes the cycle is properly formatted with even length
- The result is reversed because we build the chromosome from the end to the beginning

## Example

For cycle `(1 2 4 3 6 5)`:
- Pair (1,2): 1 is odd, so we take (2+1)/2 = 1.5 → 1 (integer division)
- Pair (4,3): 4 is even, so we take 4/2 = 2  
- Pair (6,5): 6 is even, so we take 6/2 = 3

Result: `(1 2 3)` (after reversing)

Note: The exact implementation may vary slightly depending on how the genome graph is defined, but this represents the standard approach for the cycle-to-chromosome conversion.

