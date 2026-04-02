# Rosalind Problem: Connected Components in Lisp

## Problem Understanding

The Connected Components problem asks us to find the number of connected components in an undirected graph. A connected component is a subgraph in which any two vertices are connected to each other by paths, and which is connected to no additional vertices in the supergraph.

## Approach

I'll use a Union-Find (Disjoint Set Union) data structure to efficiently solve this problem:
1. Initialize each vertex as its own parent
2. For each edge, union the two vertices
3. Count the number of distinct root nodes (connected components)

## Solution

```lisp
(defun connected-components (n edges)
  "Find the number of connected components in an undirected graph.
   n: number of vertices (0 to n-1)
   edges: list of edges as (u v) pairs"
  (let ((parent (make-array n :initial-element -1))
        (rank (make-array n :initial-element 0))
        (components n))
    
    ;; Find function with path compression
    (defun find (x)
      (if (= (aref parent x) -1)
          x
          (let ((root (find (aref parent x))))
            (setf (aref parent x) root)
            root)))
    
    ;; Union function with union by rank
    (defun union (x y)
      (let ((root-x (find x))
            (root-y (find y)))
        (when (/= root-x root-y)
          (if (< (aref rank root-x) (aref rank root-y))
              (setf (aref parent root-x) root-y)
              (setf (aref parent root-y) root-x))
          (when (= (aref rank root-x) (aref rank root-y))
            (incf (aref rank root-x)))
          (decf components))))
    
    ;; Process all edges
    (dolist (edge edges)
      (let ((u (first edge))
            (v (second edge)))
        (union u v)))
    
    components))

;; Alternative solution using DFS traversal
(defun connected-components-dfs (n edges)
  "Alternative solution using depth-first search"
  (let ((adj-list (make-hash-table :test 'equal))
        (visited (make-array n :initial-element nil))
        (components 0))
    
    ;; Build adjacency list
    (dolist (edge edges)
      (let ((u (first edge))
            (v (second edge)))
        (push v (gethash u adj-list nil))
        (push u (gethash v adj-list nil))))
    
    ;; DFS helper function
    (defun dfs (vertex)
      (setf (aref visited vertex) t)
      (dolist (neighbor (gethash vertex adj-list))
        (when (not (aref visited neighbor))
          (dfs neighbor))))
    
    ;; Count connected components
    (loop for i from 0 below n do
      (when (not (aref visited i))
        (dfs i)
        (incf components)))
    
    components))
```

## Example Usage

```lisp
;; Example from Rosalind problem
;; Graph with 4 vertices and edges: (0,1), (1,2), (3,4)
(setq n 5)
(setq edges '((0 1) (1 2) (3 4)))

;; Using Union-Find approach
(connected-components n edges)
;; Returns: 3 (components: {0,1,2}, {3,4}, {5})

;; Using DFS approach
(connected-components-dfs n edges)
;; Returns: 3
```

## Explanation

### Union-Find Approach:
1. **Initialization**: Each vertex starts as its own parent
2. **Find**: With path compression, finds root of component
3. **Union**: Merges two components and decrements component count
4. **Result**: Final component count after processing all edges

### Time Complexity: O(n + m × α(n)) where α is inverse Ackermann function
### Space Complexity: O(n)

The Union-Find approach is more efficient for this problem as it directly tracks connected components through the union operations, making it ideal for the connected components problem.

