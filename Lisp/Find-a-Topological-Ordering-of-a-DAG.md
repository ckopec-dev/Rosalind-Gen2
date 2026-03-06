# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Description
Given a directed acyclic graph (DAG), find a topological ordering of its vertices.

## Approach
I'll use Kahn's algorithm for topological sorting:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. Process vertices from queue, reducing in-degrees of neighbors
4. Add neighbors with in-degree 0 to queue
5. Continue until queue is empty

## Solution in Lisp

```lisp
(defun find-topological-ordering (graph)
  "Find a topological ordering of a directed acyclic graph.
   Graph is represented as an association list: ((vertex . (neighbors...)) ...)"
  (let ((in-degrees (make-hash-table :test 'equal))
        (queue '())
        (result '()))
    
    ;; Initialize in-degrees for all vertices
    (dolist (vertex (get-all-vertices graph))
      (setf (gethash vertex in-degrees) 0))
    
    ;; Calculate in-degrees for all vertices
    (dolist (entry graph)
      (let ((from (car entry))
            (to-list (cdr entry)))
        (dolist (to to-list)
          (incf (gethash to in-degrees)))))
    
    ;; Initialize queue with vertices having in-degree 0
    (dolist (vertex (get-all-vertices graph))
      (when (zerop (gethash vertex in-degrees))
        (push vertex queue)))
    
    ;; Process vertices in topological order
    (loop while queue do
      (let ((current (pop queue)))
        (push current result)
        (dolist (neighbor (get-neighbors graph current))
          (decf (gethash neighbor in-degrees))
          (when (zerop (gethash neighbor in-degrees))
            (push neighbor queue)))))
    
    ;; Return result in correct order (reverse since we built it backwards)
    (nreverse result)))

(defun get-all-vertices (graph)
  "Get all vertices from the graph"
  (remove-duplicates (append (mapcar #'car graph)
                            (apply #'append (mapcar #'cdr graph)))))

(defun get-neighbors (graph vertex)
  "Get neighbors of a vertex"
  (cdr (assoc vertex graph :test #'equal)))

;; Alternative implementation using explicit vertex set
(defun find-topological-ordering-alt (vertices edges)
  "Alternative implementation where vertices and edges are given separately"
  (let ((in-degrees (make-hash-table :test 'equal))
        (queue '())
        (result '()))
    
    ;; Initialize in-degrees for all vertices
    (dolist (vertex vertices)
      (setf (gethash vertex in-degrees) 0))
    
    ;; Calculate in-degrees from edges
    (dolist (edge edges)
      (let ((to (cadr edge)))
        (incf (gethash to in-degrees))))
    
    ;; Initialize queue with vertices having in-degree 0
    (dolist (vertex vertices)
      (when (zerop (gethash vertex in-degrees))
        (push vertex queue)))
    
    ;; Process vertices
    (loop while queue do
      (let ((current (pop queue)))
        (push current result)
        ;; For each outgoing edge from current vertex
        (dolist (edge edges)
          (when (equal (car edge) current)
            (let ((neighbor (cadr edge)))
              (decf (gethash neighbor in-degrees))
              (when (zerop (gethash neighbor in-degrees))
                (push neighbor queue)))))))
    
    (nreverse result)))

;; Example usage:
;; Given a graph as adjacency list: ((0 . (1 2)) (1 . (3)) (2 . (3)) (3 . (4)) (4 . ()))
;; (find-topological-ordering '((0 . (1 2)) (1 . (3)) (2 . (3)) (3 . (4)) (4 . ())))
;; Expected output: (0 1 2 3 4)

;; Example with vertices and edges separately:
;; (find-topological-ordering-alt '(0 1 2 3 4) '((0 1) (0 2) (1 3) (2 3) (3 4)))
```

## Key Points

1. **Time Complexity**: O(V + E) where V is vertices and E is edges
2. **Space Complexity**: O(V) for the hash table and queue
3. **Algorithm**: Uses Kahn's algorithm which is efficient for topological sorting
4. **Input Format**: The graph can be represented as an association list where each key is a vertex and the value is a list of its neighbors

## Test Case

For a graph with edges: 0→1, 0→2, 1→3, 2→3, 3→4
The topological ordering would be: 0 1 2 3 4

This solution handles the general case of finding a valid topological ordering of any DAG.

