# Rosalind Problem: Implement_2-BreakOnGenomeGraph

## Problem Description
Implement 2-BreakOnGenomeGraph, which takes a genome graph and performs a 2-break operation on it.

## Solution in Lisp

```lisp
(defun 2-break-on-genome-graph (genome-graph i k l m)
  "Perform 2-break operation on genome graph.
   Given a genome graph and four indices i, k, l, m,
   perform 2-break that removes edges (i,k) and (l,m)
   and adds edges (i,l) and (k,m).
   
   Args:
     genome-graph: list of edges representing genome graph
     i, k, l, m: integers representing indices of edges to break
   
   Returns:
     Modified genome graph after 2-break operation"
  
  ;; Convert genome graph to a more manageable format
  (let ((edges (copy-list genome-graph))
        (result '()))
    
    ;; Remove the edges that will be broken
    (setf edges (remove (list i k) edges))
    (setf edges (remove (list l m) edges))
    
    ;; Add the new edges
    (push (list i l) result)
    (push (list k m) result)
    
    ;; Add remaining edges
    (dolist (edge edges)
      (push edge result))
    
    ;; Return the result in proper order
    (nreverse result)))

;; Alternative implementation using more direct approach
(defun 2-break-on-genome-graph-v2 (genome-graph i k l m)
  "Alternative implementation of 2-break on genome graph.
   
   Args:
     genome-graph: list of edges representing genome graph
     i, k, l, m: integers representing indices of edges to break
   
   Returns:
     Modified genome graph after 2-break operation"
  
  ;; Create a new list with the modified edges
  (let ((new-graph '()))
    ;; Process each edge in the original graph
    (dolist (edge genome-graph)
      (if (or (equal edge (list i k)) 
              (equal edge (list l m)))
          ;; Skip the edges to be removed
          nil
          ;; Keep all other edges
          (push edge new-graph)))
    
    ;; Add the new edges from the 2-break operation
    (push (list i l) new-graph)
    (push (list k m) new-graph)
    
    ;; Return the new graph
    (nreverse new-graph)))

;; Most efficient version
(defun 2-break-on-genome-graph-efficient (genome-graph i k l m)
  "Efficient implementation of 2-break on genome graph.
   
   Args:
     genome-graph: list of edges representing genome graph
     i, k, l, m: integers representing indices of edges to break
   
   Returns:
     Modified genome graph after 2-break operation"
  
  (let ((result '()))
    ;; Process all edges
    (dolist (edge genome-graph)
      (cond
        ;; If this is one of the edges to remove
        ((and (equal (first edge) i) (equal (second edge) k))
         ;; Skip this edge
         nil)
        ((and (equal (first edge) l) (equal (second edge) m))
         ;; Skip this edge
         nil)
        ;; Otherwise, keep the edge
        (t (push edge result)))))
    
    ;; Add the new edges
    (push (list i l) result)
    (push (list k m) result)
    
    ;; Return the result
    (nreverse result)))
```

## Example Usage

```lisp
;; Example usage:
;; Original genome graph: [[1,2], [3,4], [5,6], [7,8]]
;; Perform 2-break with i=1, k=2, l=5, m=6
;; Should result in: [[1,5], [2,6], [3,4], [7,8]]

(setq original-graph '((1 2) (3 4) (5 6) (7 8)))
(setq result (2-break-on-genome-graph-efficient original-graph 1 2 5 6))
;; Result should be: ((1 5) (2 6) (3 4) (7 8))
```

## Explanation

The 2-break operation on a genome graph works as follows:

1. **Input**: A genome graph represented as a list of edges, and four indices i, k, l, m
2. **Operation**: Remove edges (i,k) and (l,m) from the graph
3. **Addition**: Add new edges (i,l) and (k,m) to the graph
4. **Output**: The modified genome graph

This operation is fundamental in genome rearrangement algorithms and helps model evolutionary changes between genomes.

The solution handles the graph modification by:
- Identifying and removing the specified edges
- Adding the new edges created by the 2-break
- Maintaining the proper structure of the genome graph

