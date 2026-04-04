# Rosalind Problem: Implement TreeColoring in Lisp

## Problem Understanding

Tree coloring involves assigning colors to nodes in a tree such that no two adjacent nodes have the same color. In this problem, we need to implement a tree coloring algorithm that assigns colors to nodes in a tree structure.

## Solution Approach

I'll implement a tree coloring algorithm using a greedy approach with depth-first search:

1. Build the tree structure from input
2. Perform DFS traversal
3. For each node, assign the smallest available color that doesn't conflict with neighbors
4. Return the color assignments

## Lisp Implementation

```lisp
(defun tree-coloring (edges n)
  "Implement tree coloring for a tree with given edges and n nodes.
   Returns a list of colors for each node (1-indexed)."
  (let ((adjacency-list (make-hash-table :test 'equal))
        (colors (make-array (1+ n) :initial-element 0))
        (visited (make-array (1+ n) :initial-element nil)))
    
    ;; Build adjacency list from edges
    (dolist (edge edges)
      (let ((u (first edge))
            (v (second edge)))
        (push v (gethash u adjacency-list (list)))
        (push u (gethash v adjacency-list (list)))))
    
    ;; DFS coloring function
    (labels ((dfs (node parent)
                (let ((available-colors (make-array (1+ n) :initial-element t)))
                  ;; Mark colors used by neighbors (excluding parent)
                  (dolist (neighbor (gethash node adjacency-list))
                    (when (and (not (= neighbor parent))
                               (> (aref colors neighbor) 0))
                      (setf (aref available-colors (aref colors neighbor)) nil)))
                  
                  ;; Find smallest available color
                  (let ((color 1))
                    (loop while (aref available-colors color) do (incf color))
                    (setf (aref colors node) color))
                  
                  ;; Recursively color neighbors
                  (dolist (neighbor (gethash node adjacency-list))
                    (when (and (not (= neighbor parent))
                               (not (aref visited neighbor)))
                      (setf (aref visited neighbor) t)
                      (dfs neighbor node))))))
      
      ;; Start DFS from node 1
      (setf (aref visited 1) t)
      (dfs 1 -1)
      
      ;; Return colors (1-indexed)
      (subseq colors 1 (1+ n)))))

;; Alternative implementation using a more direct approach
(defun tree-coloring-simple (edges n)
  "Simple tree coloring implementation."
  (let ((adjacency-list (make-hash-table :test 'equal))
        (colors (make-array (1+ n) :initial-element 0)))
    
    ;; Build adjacency list
    (dolist (edge edges)
      (let ((u (first edge))
            (v (second edge)))
        (push v (gethash u adjacency-list))
        (push u (gethash v adjacency-list)))))
    
    ;; Color nodes using greedy approach
    (loop for i from 1 to n do
      (let ((used-colors (make-array (1+ n) :initial-element nil))
            (color 1))
        ;; Mark colors used by neighbors
        (dolist (neighbor (gethash i adjacency-list))
          (when (> (aref colors neighbor) 0)
            (setf (aref used-colors (aref colors neighbor)) t)))
        
        ;; Find first available color
        (loop while (aref used-colors color) do (incf color))
        (setf (aref colors i) color)))
    
    ;; Return results (1-indexed)
    (subseq colors 1 (1+ n))))

;; Example usage:
(defun solve-tree-coloring (input-data)
  "Solve the tree coloring problem with given input."
  (let* ((lines (split-string input-data "\n"))
         (n (parse-integer (first lines)))
         (edges (rest lines))
         (edge-list (mapcar (lambda (line)
                              (let ((parts (split-string line " ")))
                                (list (parse-integer (first parts))
                                      (parse-integer (second parts)))))
                            edges)))
    (tree-coloring-simple edge-list n)))

;; Helper function to split strings (if not available)
(defun split-string (string delimiter)
  "Split a string by delimiter."
  (remove-if (lambda (s) (string= s ""))
             (split-string-helper string delimiter 0)))

(defun split-string-helper (string delimiter start)
  "Helper function for splitting strings."
  (let ((pos (search delimiter string :start2 start)))
    (if pos
        (cons (subseq string start pos)
              (split-string-helper string delimiter (+ pos (length delimiter))))
        (list (subseq string start)))))

;; Example test case
(defun test-tree-coloring ()
  "Test the tree coloring implementation."
  (let ((edges '((1 2) (2 3) (3 4) (4 5)))
        (n 5))
    (print (tree-coloring-simple edges n))))

;; For Rosalind format, assuming input is in the format:
;; n
;; u1 v1
;; u2 v2
;; ...
(defun rosalind-tree-coloring (input-string)
  "Process Rosalind-style input for tree coloring."
  (let* ((lines (split-string input-string "\n"))
         (n (parse-integer (first lines)))
         (edge-lines (rest lines))
         (edges (mapcar (lambda (line)
                          (let ((parts (split-string line " ")))
                            (list (parse-integer (first parts))
                                  (parse-integer (second parts)))))
                        edge-lines)))
    (let ((result (tree-coloring-simple edges n)))
      (format t "~{~a ~}~%" result))))

;; Sample input format for Rosalind:
;; 5
;; 1 2
;; 2 3
;; 3 4
;; 4 5
```

## Key Features of the Implementation

1. **Graph Representation**: Uses adjacency list representation for efficient neighbor access
2. **Greedy Coloring**: Assigns the smallest available color to each node
3. **Neighbor Conflict Checking**: Ensures no adjacent nodes have the same color
4. **Tree-Specific Optimization**: Leverages tree properties for efficient coloring

## Time Complexity
- **Time**: O(n × d) where n is the number of nodes and d is the maximum degree
- **Space**: O(n + m) where m is the number of edges

## Usage Example

For a tree with edges (1,2), (2,3), (3,4), (4,5):
- Node 1: Color 1
- Node 2: Color 2  
- Node 3: Color 1
- Node 4: Color 2
- Node 5: Color 1

This implementation efficiently solves the tree coloring problem for Rosalind's requirements.

