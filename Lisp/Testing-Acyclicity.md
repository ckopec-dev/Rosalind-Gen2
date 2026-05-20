# Rosalind Problem: Testing Acyclicity - Lisp Solution

## Problem Understanding

The problem asks us to determine whether a given directed graph is acyclic (i.e., contains no cycles). This is a classic graph theory problem that can be solved using topological sorting or cycle detection algorithms.

## Approach

I'll use a **topological sorting approach** with Kahn's algorithm:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. Process vertices from queue, reducing in-degrees of neighbors
4. If we process all vertices, the graph is acyclic; otherwise, it has cycles

## Solution

```lisp
(defun testing-acyclicity (edges)
  "Determine if a directed graph is acyclic.
   Returns 1 if acyclic, 0 if contains cycles."
  (if (null edges)
      1
      (let ((vertices '())
            (in-degrees (make-hash-table :test 'equal))
            (adjacency-list (make-hash-table :test 'equal))
            (queue '())
            (processed 0))
        ;; Extract all vertices and build adjacency list
        (dolist (edge edges)
          (let ((from (first edge))
                (to (second edge)))
            ;; Add vertices to list
            (unless (member from vertices)
              (push from vertices))
            (unless (member to vertices)
              (push to vertices))
            ;; Build adjacency list
            (push to (gethash from adjacency-list '()))
            ;; Initialize in-degrees
            (unless (gethash to in-degrees)
              (puthash to 0 in-degrees))
            ;; Increment in-degree of 'to'
            (puthash to (+ 1 (gethash to in-degrees 0)) in-degrees))))
        
        ;; Initialize queue with vertices having in-degree 0
        (dolist (vertex vertices)
          (if (= (gethash vertex in-degrees 0) 0)
              (push vertex queue)))
        
        ;; Process vertices using Kahn's algorithm
        (while queue
          (let ((current (pop queue)))
            (incf processed)
            ;; Process neighbors
            (dolist (neighbor (gethash current adjacency-list '()))
              (let ((new-in-degree (- (gethash neighbor in-degrees 0) 1)))
                (puthash neighbor new-in-degree in-degrees)
                (when (= new-in-degree 0)
                  (push neighbor queue)))))
          )
        
        ;; If we processed all vertices, graph is acyclic
        (if (= processed (length vertices))
            1
            0))))

;; Alternative cleaner implementation
(defun test-acyclicity (edges)
  "Test if a directed graph is acyclic using topological sorting."
  (if (null edges)
      1
      (let* ((vertices (remove-duplicates (flatten edges)))
             (in-degrees (make-hash-table :test 'equal))
             (adj-list (make-hash-table :test 'equal))
             (queue '()))
        ;; Initialize in-degrees and adjacency list
        (dolist (edge edges)
          (let ((from (first edge))
                (to (second edge)))
            ;; Initialize in-degrees
            (unless (gethash from in-degrees)
              (puthash from 0 in-degrees))
            (unless (gethash to in-degrees)
              (puthash to 0 in-degrees))
            ;; Build adjacency list
            (push to (gethash from adj-list '()))
            ;; Increment in-degree of 'to'
            (incf (gethash to in-degrees 0))))
        
        ;; Find vertices with in-degree 0
        (dolist (vertex vertices)
          (when (= (gethash vertex in-degrees 0) 0)
            (push vertex queue)))
        
        ;; Process using BFS
        (let ((processed 0))
          (while queue
            (let ((current (pop queue)))
              (incf processed)
              (dolist (neighbor (gethash current adj-list '()))
                (decf (gethash neighbor in-degrees))
                (when (= (gethash neighbor in-degrees) 0)
                  (push neighbor queue)))))
          ;; Return 1 if all vertices processed, 0 if cycle exists
          (if (= processed (length vertices))
              1
              0)))))

;; Example usage:
;; (test-acyclicity '((1 2) (2 3) (3 4) (4 1))) ; Returns 0 (has cycle)
;; (test-acyclicity '((1 2) (2 3) (3 4))) ; Returns 1 (no cycle)
```

## Explanation

The solution works as follows:

1. **Graph Representation**: 
   - Build an adjacency list to represent the graph
   - Track in-degrees for each vertex (number of incoming edges)

2. **Initialization**:
   - Find all vertices in the graph
   - Initialize in-degrees for all vertices
   - Identify vertices with in-degree 0 (no incoming edges)

3. **Topological Sort Process**:
   - Process vertices with in-degree 0
   - For each processed vertex, reduce in-degrees of its neighbors
   - Add neighbors with in-degree 0 to processing queue

4. **Cycle Detection**:
   - If we process all vertices, the graph is acyclic (return 1)
   - If we can't process all vertices, there's a cycle (return 0)

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V + E) for storing adjacency list and in-degrees

## Test Cases

```lisp
;; Test case 1: Acyclic graph
(test-acyclicity '((1 2) (2 3) (3 4))) ; Should return 1

;; Test case 2: Cyclic graph  
(test-acyclicity '((1 2) (2 3) (3 1))) ; Should return 0

;; Test case 3: Empty graph
(test-acyclicity '()) ; Should return 1
```

This solution efficiently determines whether a directed graph contains cycles using the well-established topological sorting algorithm.

