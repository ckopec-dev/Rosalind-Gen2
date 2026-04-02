# Finding a Shared Motif - Rosalind Problem Solution in Lisp

## Problem Understanding

The problem asks us to find the longest common substring among all given DNA sequences. This is a classic bioinformatics problem that requires finding the maximum shared motif (substring) across multiple sequences.

## Approach

1. Generate all possible substrings from the first sequence
2. For each substring, check if it exists in all other sequences
3. Keep track of the longest common substring found

## Solution

```lisp
(defun get-substrings (str start end)
  "Get all substrings of str from start to end"
  (if (> start end)
      nil
      (cons (subseq str start (1+ end))
            (get-substrings str (1+ start) end))))


(defun get-all-substrings (str)
  "Get all substrings of a string"
  (let ((len (length str)))
    (if (= len 0)
        nil
        (let ((substrings '()))
          (loop for i from 0 to (1- len) do
                (loop for j from i to (1- len) do
                      (push (subseq str i (1+ j)) substrings))))
          (remove-duplicates substrings)))))

(defun is-substring-in-all (substring sequences)
  "Check if substring exists in all sequences"
  (every (lambda (seq) (search substring seq)) sequences))

(defun find-shared-motif (sequences)
  "Find the longest shared motif among all sequences"
  (if (null sequences)
      ""
      (let ((first-seq (first sequences)))
        (let ((substrings (get-all-substrings first-seq)))
          (let ((common-substrings 
                 (remove-if-not 
                  (lambda (sub) (is-substring-in-all sub sequences))
                  substrings)))
            (if (null common-substrings)
                ""
                (reduce (lambda (a b) 
                          (if (> (length a) (length b)) a b))
                        common-substrings)))))))

;; Alternative more efficient approach
(defun find-longest-common-substring (sequences)
  "Find longest common substring using a more efficient approach"
  (if (null sequences)
      ""
      (let ((first-seq (first sequences))
            (rest-sequences (rest sequences)))
        (let ((longest-motif ""))
          (loop for i from 0 to (1- (length first-seq)) do
                (loop for j from i to (1- (length first-seq)) do
                      (let ((motif (subseq first-seq i (1+ j))))
                        (if (and (> (length motif) (length longest-motif))
                                 (every (lambda (seq) 
                                          (search motif seq)) 
                                        rest-sequences))
                            (setf longest-motif motif)))))
          longest-motif))))

;; Read input from file or stdin
(defun read-fasta-sequences ()
  "Read FASTA formatted sequences"
  (let ((sequences '())
        (current-seq ""))
    (loop for line = (read-line nil nil nil)
          while line do
          (if (char= (first line) #\>)
              (progn
                (when current-seq
                  (push current-seq sequences))
                (setf current-seq ""))
              (setf current-seq (concatenate 'string current-seq line))))
    (when current-seq
      (push current-seq sequences))
    (nreverse sequences)))

;; Main function
(defun solve-finding-shared-motif ()
  "Main function to solve the problem"
  (let ((sequences (read-fasta-sequences)))
    (format t "~A~%" (find-longest-common-substring sequences))))

;; Example usage with sample data
(defun test-example ()
  "Test with example sequences"
  (let ((test-sequences '("CATTGCTGAA"
                          "GCGCGTTGCC"
                          "GAGCGTTGAA"
                          "GAGCGTTGAA")))
    (format t "Longest shared motif: ~A~%" 
            (find-longest-common-substring test-sequences))))

;; Run the test
(test-example)
```

## Explanation

The solution implements two approaches:

1. **First approach**: Generate all substrings of the first sequence, then check each one against all other sequences
2. **Second approach**: More efficient - iterate through substrings of the first sequence and check if they exist in all other sequences

## Key Functions

- `get-all-substrings`: Generates all possible substrings of a string
- `is-substring-in-all`: Checks if a substring exists in all sequences
- `find-longest-common-substring`: Main function that finds the longest common motif
- `read-fasta-sequences`: Reads FASTA formatted input

## Time Complexity

- **First approach**: O(n³) where n is the length of the sequences
- **Second approach**: O(n²) in average case, better for large inputs

## Usage

```lisp
;; For testing with sample data
(test-example)

;; For actual problem solving
(solve-finding-shared-motif)
```

The solution handles multiple DNA sequences and returns the longest substring that appears in all of them, which is exactly what the Rosalind problem requires.

