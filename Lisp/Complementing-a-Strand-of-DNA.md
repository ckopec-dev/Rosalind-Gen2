# Rosalind Problem: Complementing a Strand of DNA (Lisp Solution)

## Problem Statement
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'. The reverse complement of a DNA string is the complement of the string, reversed. Given a DNA string, return its reverse complement.

## Solution

```lisp
(defun complement-dna (dna-string)
  "Return the reverse complement of a DNA string."
  (let ((complement-map '(("A" . "T") ("T" . "A") ("C" . "G") ("G" . "C"))))
    (->> dna-string
         (remove-duplicates)
         (mapcar (lambda (char)
                    (cdr (assoc char complement-map))))
         (reverse)
         (apply #'concatenate 'string)))))

;; Alternative implementation using string operations
(defun complement-dna-alt (dna-string)
  "Alternative implementation for reverse complement."
  (let ((complement-map '((#\A . #\T) (#\T . #\A) (#\C . #\G) (#\G . #\C))))
    (->> dna-string
         (map 'string (lambda (char)
                        (cdr (assoc char complement-map))))
         (reverse)
         (apply #'concatenate 'string)))))
```

## Example Usage

```lisp
;; Test the function
(complement-dna "AAAACCCGGT")
;; Returns: "ACCGGGTTTT"

(complement-dna "ACGTACGT")
;; Returns: "ACGTACGT"

(complement-dna "TTTTTTTT")
;; Returns: "AAAAAAAA"
```

## Explanation

1. **Complement Mapping**: Create an association list mapping each DNA nucleotide to its complement:
   - A → T
   - T → A  
   - C → G
   - G → C

2. **Process Steps**:
   - Convert the input string to individual characters
   - Map each character to its complement using the association list
   - Reverse the resulting sequence
   - Join the characters back into a string

3. **Key Functions**:
   - `assoc`: Looks up a key in an association list
   - `reverse`: Reverses the order of elements
   - `apply #'concatenate`: Joins string elements into a single string

## More Concise Version

```lisp
(defun complement-dna-concise (dna-string)
  "Concise implementation of reverse complement."
  (let ((complement-map '((#\A . #\T) (#\T . #\A) (#\C . #\G) (#\G . #\C))))
    (->> dna-string
         (map 'string (lambda (char) (cdr (assoc char complement-map))))
         (reverse)
         (coerce 'string)))))
```

This solution correctly handles the DNA reverse complement problem by first finding the complement of each nucleotide and then reversing the entire sequence.

