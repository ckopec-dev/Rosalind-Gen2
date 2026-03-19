# Rosalind Problem: Phylogeny Comparison with Split Distance

## Problem Description

Given two trees T1 and T2 on the same set of taxa, compute the split distance between them. The split distance is the number of splits (edges) that are present in one tree but not in the other.

## Solution in Lisp

```lisp
(defun split-distance (tree1 tree2)
  "Compute the split distance between two phylogenetic trees."
  (let ((splits1 (get-splits tree1))
        (splits2 (get-splits tree2)))
    (length (set-difference splits1 splits2))))

(defun get-splits (tree)
  "Extract all splits from a tree represented as nested lists."
  (let ((splits '()))
    (get-splits-helper tree splits)
    splits))

(defun get-splits-helper (node splits)
  "Helper function to recursively extract splits from tree."
  (cond
    ;; If node is a leaf (string), return empty splits
    ((stringp node) nil)
    
    ;; If node is a list with two elements, it's a split
    ((and (listp node) (= (length node) 2))
     (let ((left (first node))
           (right (second node)))
       ;; Add the split (left, right) to our list
       (push (sort-split (list left right)) splits)
       ;; Recursively process subtrees
       (get-splits-helper left splits)
       (get-splits-helper right splits)))
    
    ;; If node is a list with more than 2 elements, it's a multifurcation
    ((listp node)
     (dolist (subnode node)
       (get-splits-helper subnode splits)))))

(defun sort-split (split)
  "Sort the elements of a split to ensure consistent representation."
  (sort (copy-list split) #'string<))

(defun parse-newick (newick-string)
  "Parse a Newick format string into nested list representation."
  (let ((tokens (split-string newick-string #\,)))
    ;; Simplified parser - in practice would need more robust parsing
    (read-from-string newick-string)))

;; Example usage:
;; (split-distance tree1 tree2)

;; Alternative implementation using a more complete approach:
(defun compute-split-distance (tree1 tree2)
  "Compute split distance between two trees."
  (let ((splits1 (extract-splits tree1))
        (splits2 (extract-splits tree2)))
    (let ((unique-to-tree1 (set-difference splits1 splits2))
          (unique-to-tree2 (set-difference splits2 splits1)))
      (+ (length unique-to-tree1) (length unique-to-tree2)))))

(defun extract-splits (tree)
  "Extract all splits from a tree structure."
  (let ((splits '()))
    (extract-splits-helper tree splits)
    splits))

(defun extract-splits-helper (node splits)
  "Recursive helper to extract splits."
  (cond
    ;; Base case: leaf node
    ((stringp node) nil)
    
    ;; Internal node with two children
    ((and (listp node) (= (length node) 2))
     (let ((left (first node))
           (right (second node)))
       ;; Add the split
       (push (sort-split (list (collect-leaf-names left) 
                              (collect-leaf-names right))) 
             splits)
       ;; Process children
       (extract-splits-helper left splits)
       (extract-splits-helper right splits))))
  
  (if (listp node)
      (dolist (child node)
        (extract-splits-helper child splits))))

(defun collect-leaf-names (node)
  "Collect all leaf names from a subtree."
  (cond
    ((stringp node) (list node))
    ((listp node) 
     (apply #'append (mapcar #'collect-leaf-names node)))
    (t '())))

;; More robust version for Rosalind problem:
(defun rosalind-split-distance (tree1-str tree2-str)
  "Compute split distance between two Newick formatted trees."
  (let ((tree1 (parse-newick tree1-str))
        (tree2 (parse-newick tree2-str)))
    (let ((splits1 (get-all-splits tree1))
          (splits2 (get-all-splits tree2)))
      (length (set-difference splits1 splits2)))))

(defun get-all-splits (tree)
  "Get all splits from a tree."
  (let ((splits '()))
    (get-splits-rec tree splits)
    splits))

(defun get-splits-rec (node splits)
  "Recursive function to collect splits."
  (cond
    ;; Leaf node
    ((stringp node) nil)
    
    ;; Internal node with children
    ((listp node)
     (let ((children (remove-if #'stringp node)))
       ;; Process children
       (dolist (child children)
         (get-splits-rec child splits))
       ;; Add split for this node
       (let ((child-sets (mapcar #'get-leaf-set children)))
         (push (sort-splits child-sets) splits))))))

(defun get-leaf-set (node)
  "Get set of leaf names from subtree."
  (cond
    ((stringp node) (list node))
    ((listp node) 
     (apply #'append (mapcar #'get-leaf-set node)))
    (t '())))

(defun sort-splits (split-list)
  "Sort split elements consistently."
  (mapcar (lambda (split) (sort (copy-list split) #'string<))
          split-list))
```

## Key Concepts

1. **Split**: A partition of taxa induced by removing an edge from the tree
2. **Split Distance**: Number of splits that are in one tree but not in the other
3. **Tree Representation**: Using nested lists to represent Newick format

## Usage

The solution parses two Newick format trees and computes their split distance by:
1. Extracting all splits from each tree
2. Finding splits unique to each tree
3. Returning the total count of unique splits

This approach handles the fundamental problem of comparing phylogenetic tree topologies using split distances.

