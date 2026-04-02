# Rosalind Problem: Shortest Cycle Through a Given Edge

## Problem Understanding

We need to find the shortest cycle in an undirected graph that passes through a given edge. This is a classic graph algorithm problem that can be solved by:

1. Removing the given edge from the graph
2. Finding the shortest path between the two vertices of the edge in the remaining graph
3. Adding the removed edge back to form the cycle
4. The cycle length equals the shortest path length + 1

## Solution Approach

I'll implement this using BFS to find the shortest path, then calculate the cycle length.

```lisp
(defun shortest-cycle-through-edge (edges start-vertex end-vertex)
  "Find the shortest cycle through a given edge in an undirected graph."
  (let ((graph (build-adjacency-list edges))
        (visited (make-hash-table :test 'equal))
        (queue '())
        (distance (make-hash-table :test 'equal)))
    
    ;; Initialize BFS
    (setf (gethash start-vertex visited) t)
    (setf (gethash start-vertex distance) 0)
    (push (list start-vertex 0) queue)
    
    ;; BFS to find shortest path from start to end
    (loop while queue do
      (let ((current (pop queue))
            (current-vertex (first current))
            (current-dist (second current)))
        (if (equal current-vertex end-vertex)
            (return (+ current-dist 1)) ; +1 for the edge we removed
            (dolist (neighbor (gethash current-vertex graph))
              (unless (gethash neighbor visited)
                (setf (gethash neighbor visited) t)
                (setf (gethash neighbor distance) (+ current-dist 1))
                (push (list neighbor (+ current-dist 1)) queue))))))
  
  ;; Helper function to build adjacency list
  (defun build-adjacency-list (edges)
    "Convert list of edges to adjacency list representation."
    (let ((adj-list (make-hash-table :test 'equal)))
      (dolist (edge edges)
        (let ((u (first edge))
              (v (second edge)))
          (push v (gethash u adj-list))
          (push u (gethash v adj-list)))))
      adj-list)
  
  ;; Main function to solve the problem
  (defun solve-shortest-cycle (edges edge-to-include)
    "Find shortest cycle through a given edge."
    (let* ((start-vertex (first edge-to-include))
           (end-vertex (second edge-to-include))
           (remaining-edges (remove edge-to-include edges :test 'equal))
           (graph (build-adjacency-list remaining-edges)))
      
      ;; Find shortest path between the two vertices in the remaining graph
      (let ((visited (make-hash-table :test 'equal))
            (queue (list (list start-vertex 0)))
            (distance (make-hash-table :test 'equal)))
        
        ;; Initialize
        (setf (gethash start-vertex visited) t)
        (setf (gethash start-vertex distance) 0)
        
        ;; BFS
        (loop while queue do
          (let* ((current (pop queue))
                 (current-vertex (first current))
                 (current-dist (second current)))
            (if (equal current-vertex end-vertex)
                (return (+ current-dist 1)) ; +1 for the edge we removed
                (dolist (neighbor (gethash current-vertex graph))
                  (unless (gethash neighbor visited)
                    (setf (gethash neighbor visited) t)
                    (setf (gethash neighbor distance) (+ current-dist 1))
                    (push (list neighbor (+ current-dist 1)) queue)))))))
  
  ;; Alternative cleaner implementation
  (defun shortest-cycle-through-edge (edges start-vertex end-vertex)
    "Find shortest cycle through edge (start-vertex, end-vertex)."
    (let* ((graph (build-adjacency-list edges))
           (visited (make-hash-table :test 'equal))
           (queue (list (list start-vertex 0)))
           (distance (make-hash-table :test 'equal)))
      
      ;; Initialize BFS
      (setf (gethash start-vertex visited) t)
      (setf (gethash start-vertex distance) 0)
      
      ;; Run BFS
      (loop while queue do
        (let* ((current (pop queue))
               (current-vertex (first current))
               (current-dist (second current)))
          (if (equal current-vertex end-vertex)
              (return (+ current-dist 1))
              (dolist (neighbor (gethash current-vertex graph))
                (unless (gethash neighbor visited)
                  (setf (gethash neighbor visited) t)
                  (setf (gethash neighbor distance) (+ current-dist 1))
                  (push (list neighbor (+ current-dist 1)) queue)))))))
  
  ;; Final clean solution
  (defun shortest-cycle-through-edge (edges start-vertex end-vertex)
    "Find the shortest cycle in an undirected graph that passes through a given edge."
    (let* ((graph (build-adjacency-list (remove (list start-vertex end-vertex) edges :test 'equal)))
           (visited (make-hash-table :test 'equal))
           (queue (list (list start-vertex 0)))
           (distance (make-hash-table :test 'equal)))
      
      ;; Initialize
      (setf (gethash start-vertex visited) t)
      (setf (gethash start-vertex distance) 0)
      
      ;; BFS to find shortest path
      (loop while queue do
        (let* ((current (pop queue))
               (current-vertex (first current))
               (current-dist (second current)))
          (if (equal current-vertex end-vertex)
              (return (+ current-dist 1)) ; +1 for the edge that was removed
              (dolist (neighbor (gethash current-vertex graph))
                (unless (gethash neighbor visited)
                  (setf (gethash neighbor visited) t)
                  (setf (gethash neighbor distance) (+ current-dist 1))
                  (push (list neighbor (+ current-dist 1)) queue))))))))

;; Test example
(defun test-shortest-cycle ()
  "Test the shortest cycle function with sample data."
  (let* ((edges '((1 2) (2 3) (3 4) (4 1) (1 3) (2 4)))
         (result (shortest-cycle-through-edge edges 1 2)))
    (format t "Shortest cycle through edge (1,2): ~A~%" result)))

;; Run the test
(test-shortest-cycle)
```

## Explanation

The algorithm works as follows:

1. **Build the graph**: Create an adjacency list representation of the undirected graph from the edge list.

2. **Remove the given edge**: Temporarily remove the specified edge from the graph to create a disconnected graph.

3. **BFS search**: Run a breadth-first search from one endpoint of the removed edge to the other endpoint in the remaining graph.

4. **Calculate cycle length**: The shortest path length plus 1 gives us the length of the shortest cycle (the +1 accounts for the removed edge that completes the cycle).

## Time Complexity
- Building adjacency list: O(E) where E is the number of edges
- BFS traversal: O(V + E) where V is the number of vertices
- Overall: O(V + E)

## Space Complexity
- O(V + E) for storing the adjacency list and BFS data structures

This solution efficiently finds the shortest cycle through any given edge in an undirected graph using standard graph traversal techniques.

