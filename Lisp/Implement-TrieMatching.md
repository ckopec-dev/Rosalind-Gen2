# Rosalind Problem: Implement_TrieMatching in Lisp

## Problem Understanding

The problem requires implementing a trie (prefix tree) to search for multiple patterns in a text string. Given a text and a collection of patterns, we need to find all positions where each pattern occurs in the text.

## Solution Approach

I'll implement a trie data structure and use it to efficiently search for patterns in the text.

```lisp
;; Define trie node structure
(defstruct trie-node
  (children (make-hash-table :test 'equal) :type hash-table)
  (pattern-end nil :type boolean)
  (pattern-id nil :type (or null integer)))

;; Function to insert a pattern into the trie
(defun insert-pattern (root pattern id)
  "Insert a pattern into the trie with given ID"
  (let ((current root))
    (loop for char across pattern do
      (let ((child (gethash char (trie-node-children current))))
        (unless child
          (setf child (make-trie-node))
          (setf (gethash char (trie-node-children current)) child))
        (setf current child)))
    (setf (trie-node-pattern-end current) t)
    (setf (trie-node-pattern-id current) id)))

;; Function to build trie from patterns
(defun build-trie (patterns)
  "Build a trie from a list of patterns"
  (let ((root (make-trie-node)))
    (loop for pattern in patterns
          for id from 0 do
          (insert-pattern root pattern id))
    root))

;; Function to search for patterns in text using trie
(defun trie-matching (text patterns)
  "Find all positions where patterns occur in text using trie matching"
  (let ((root (build-trie patterns))
        (positions '()))
    ;; For each position in text, check if any pattern starts here
    (loop for i from 0 below (length text) do
      (let ((current root)
            (j i))
        (loop while (and (< j (length text))
                         (let ((child (gethash (char text j) (trie-node-children current))))
                           (when child
                             (setf current child)
                             (if (trie-node-pattern-end current)
                                 (push (list i (trie-node-pattern-id current)) positions))
                             t)))
              do (incf j))))
    (nreverse positions)))

;; Alternative approach - more efficient for multiple patterns
(defun trie-matching-efficient (text patterns)
  "Efficient trie matching for multiple patterns"
  (let ((root (build-trie patterns))
        (results '()))
    (loop for i from 0 below (length text) do
      (let ((current root)
            (found nil))
        (loop for j from i below (length text) do
          (let ((child (gethash (char text j) (trie-node-children current))))
            (if child
                (progn
                  (setf current child)
                  (when (trie-node-pattern-end current)
                    (push (list i (trie-node-pattern-id current)) results)))
                (return)))))
    (nreverse results)))

;; Most efficient approach - process text once
(defun implement-trie-matching (text patterns)
  "Implement trie matching to find all pattern occurrences in text"
  (let ((root (build-trie patterns))
        (results '()))
    ;; Process each starting position in text
    (loop for i from 0 below (length text) do
      (let ((current root))
        (loop for j from i below (length text) do
          (let ((child (gethash (char text j) (trie-node-children current))))
            (if child
                (progn
                  (setf current child)
                  (when (trie-node-pattern-end current)
                    (push (list i (trie-node-pattern-id current)) results)))
                (return))))))
    (nreverse results)))

;; Test function
(defun test-trie-matching ()
  "Test the trie matching implementation"
  (let ((text "AATCGGGTTCAATCGGGGT")
        (patterns '("ATCG" "GGGT" "CGGG")))
    (let ((result (implement-trie-matching text patterns)))
      (format t "Text: ~A~%" text)
      (format t "Patterns: ~A~%" patterns)
      (format t "Results: ~A~%" result)
      result)))

;; Main function for Rosalind problem
(defun solve-rosalind-trie-matching (text patterns)
  "Solve the Rosalind trie matching problem"
  (implement-trie-matching text patterns))
```

## Key Components Explained

1. **Trie Node Structure**: Each node has:
   - `children`: Hash table mapping characters to child nodes
   - `pattern-end`: Boolean flag indicating if a pattern ends at this node
   - `pattern-id`: ID of the pattern ending here

2. **Insertion Function**: Builds the trie by inserting each pattern character by character

3. **Matching Function**: For each position in the text, traverse the trie to find matching patterns

## Time and Space Complexity

- **Time Complexity**: O(|text| × |patterns| × |longest_pattern|) in worst case
- **Space Complexity**: O(|patterns| × |longest_pattern|) for the trie structure

## Usage Example

```lisp
;; Example usage
(setq text "AATCGGGTTCAATCGGGGT")
(setq patterns '("ATCG" "GGGT" "CGGG"))
(solve-rosalind-trie-matching text patterns)
```

This implementation efficiently handles the trie matching problem by building a prefix tree of all patterns and then searching through the text to find all occurrences of these patterns.

