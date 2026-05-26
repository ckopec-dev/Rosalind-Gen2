# Rosalind Problem: Consensus and Profile in Lisp

## Problem Understanding

Given a collection of DNA strings of equal length, we need to:
1. Create a profile matrix showing nucleotide frequencies at each position
2. Generate the consensus string (most frequent nucleotide at each position)

## Solution Approach

I'll implement this step by step:
1. Parse input DNA strings
2. Build profile matrix
3. Generate consensus string
4. Output results

## Lisp Implementation

```lisp
(defun parse-dna-strings (input)
  "Parse DNA strings from input (assuming FASTA format)"
  (let ((dna-strings '())
        (current-string ""))
    (dolist (line (split-string input "\n"))
      (if (and (not (string= line "")) 
               (not (char= (char line 0) #\>)))
          (setf current-string (concatenate 'string current-string line))
          (if (and (not (string= line "")) 
                   (char= (char line 0) #\>))
              (when (not (string= current-string ""))
                (push current-string dna-strings)
                (setf current-string ""))
              (when (not (string= current-string ""))
                (push current-string dna-strings)
                (setf current-string "")))))
    (when (not (string= current-string ""))
      (push current-string dna-strings))
    (nreverse dna-strings)))

(defun build-profile-matrix (dna-strings)
  "Build profile matrix for given DNA strings"
  (when dna-strings
    (let* ((length (length (first dna-strings)))
           (profile (make-array '(4 length) :element-type 'fixnum :initial-element 0))
           (nucleotides '(#\A #\C #\G #\T)))
      (dolist (dna-string dna-strings)
        (loop for i from 0 below length do
          (let ((nucleotide (char dna-string i)))
            (case nucleotide
              (#\A (incf (aref profile 0 i)))
              (#\C (incf (aref profile 1 i)))
              (#\G (incf (aref profile 2 i)))
              (#\T (incf (aref profile 3 i)))))))
      profile)))

(defun get-consensus-string (profile)
  "Generate consensus string from profile matrix"
  (let* ((length (array-dimension profile 1))
         (consensus-string (make-array length :element-type 'character))
         (nucleotides '(#\A #\C #\G #\T)))
    (loop for i from 0 below length do
      (let ((max-count 0)
            (max-nucleotide #\A))
        (loop for j from 0 below 4 do
          (let ((count (aref profile j i)))
            (when (> count max-count)
              (setf max-count count
                    max-nucleotide (nth j nucleotides)))))
        (setf (aref consensus-string i) max-nucleotide)))
    (coerce consensus-string 'string)))

(defun format-profile-output (profile)
  "Format profile matrix for output"
  (let ((nucleotides '(#\A #\C #\G #\T)))
    (loop for i from 0 below 4 do
      (format t "~A: " (nth i nucleotides))
      (loop for j from 0 below (array-dimension profile 1) do
        (format t "~A " (aref profile i j)))
      (format t "~%"))))

(defun consensus-and-profile (dna-strings)
  "Main function to solve consensus and profile problem"
  (let ((profile (build-profile-matrix dna-strings))
        (consensus (get-consensus-string (build-profile-matrix dna-strings))))
    (format t "~A~%" consensus)
    (format-profile-output profile)))

;; Alternative cleaner implementation
(defun solve-consensus-profile (dna-strings)
  "Solve consensus and profile problem with cleaner approach"
  (when dna-strings
    (let* ((length (length (first dna-strings)))
           (profile (make-array '(4 length) :element-type 'fixnum :initial-element 0))
           (consensus-string (make-string length)))
      
      ;; Build profile matrix
      (dolist (dna-string dna-strings)
        (loop for i from 0 below length do
          (let ((nucleotide (char dna-string i)))
            (case nucleotide
              (#\A (incf (aref profile 0 i)))
              (#\C (incf (aref profile 1 i)))
              (#\G (incf (aref profile 2 i)))
              (#\T (incf (aref profile 3 i)))))))
      
      ;; Generate consensus string
      (loop for i from 0 below length do
        (let ((max-count 0)
              (max-nucleotide #\A))
          (loop for j from 0 below 4 do
            (let ((count (aref profile j i)))
              (when (> count max-count)
                (setf max-count count
                      max-nucleotide (char-downcase (nth j '(#\A #\C #\G #\T)))))))
          (setf (char consensus-string i) max-nucleotide)))
      
      ;; Output results
      (format t "~A~%" consensus-string)
      (format t "A: ")
      (loop for i from 0 below (array-dimension profile 1) do
        (format t "~A " (aref profile 0 i)))
      (format t "~%")
      (format t "C: ")
      (loop for i from 0 below (array-dimension profile 1) do
        (format t "~A " (aref profile 1 i)))
      (format t "~%")
      (format t "G: ")
      (loop for i from 0 below (array-dimension profile 1) do
        (format t "~A " (aref profile 2 i)))
      (format t "~%")
      (format t "T: ")
      (loop for i from 0 below (array-dimension profile 1) do
        (format t "~A " (aref profile 3 i)))
      (format t "~%"))))

;; Example usage:
;; (solve-consensus-profile '("ATCCAGCT", "GGGCAACT", "ATGGATCT", "AAGCAACC", "TTGGAACT", "ATGCCATT", "ATGGCACT"))
```

## Example Input/Output

**Input:**
```
ATCCAGCT
GGGCAACT
ATGGATCT
AAGCAACC
TTGGAACT
ATGCCATT
ATGGCACT
```

**Output:**
```
ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6
```

## Key Features of the Solution

1. **Profile Matrix Construction**: Creates a 4×n matrix where rows represent nucleotides (A, C, G, T) and columns represent positions
2. **Consensus String Generation**: For each position, selects the nucleotide with maximum frequency
3. **Efficient Processing**: Single pass through DNA strings to build profile, single pass to generate consensus
4. **Clean Output Format**: Follows Rosalind's expected output format

## Time and Space Complexity

- **Time Complexity**: O(n×m) where n is number of DNA strings and m is string length
- **Space Complexity**: O(4×m) for the profile matrix

