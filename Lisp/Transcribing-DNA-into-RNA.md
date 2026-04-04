# Rosalind Problem: Transcribing DNA into RNA

## Problem Statement
In RNA strings, symbols 'U' and 'T' are used to represent uracil and thymine respectively. Given a DNA string, we need to transcribe it into RNA by replacing all occurrences of 'T' with 'U'.

## Solution in Lisp

```lisp
(defun transcribe-dna-to-rna (dna-string)
  "Transcribes a DNA string to RNA by replacing all 'T' with 'U'"
  (let ((rna-string ""))
    (loop for char across dna-string
          do (if (char= char #\T)
                 (setf rna-string (concatenate 'string rna-string "U"))
                 (setf rna-string (concatenate 'string rna-string (string char)))))
    rna-string))

;; Alternative more concise solution using replace
(defun transcribe-dna-to-rna-v2 (dna-string)
  "Transcribes DNA to RNA using string replacement"
  (replace dna-string "T" "U"))

;; Even more functional approach
(defun transcribe-dna-to-rna-v3 (dna-string)
  "Transcribes DNA to RNA using map and cond"
  (list-to-string
   (map 'list 
        (lambda (char)
          (if (char= char #\T)
              #\U
              char))
        (string-to-list dna-string))))

;; Example usage:
;; (transcribe-dna-to-rna "GATGGAACTTGACTACGTAAATT")
;; Expected output: "GAUGGAACUUGACUACGUAAAUU"
```

## Explanation

The problem requires converting a DNA string to an RNA string by replacing all 'T' nucleotides with 'U' nucleotides.

### Approach 1: Iterative with string concatenation
- Loop through each character of the DNA string
- If character is 'T', append 'U' to result
- Otherwise, append the original character
- Return the final RNA string

### Approach 2: Using built-in replace function
- Simple one-liner using the built-in `replace` function
- Replaces all occurrences of 'T' with 'U'

### Approach 3: Functional approach
- Uses `map` to transform each character
- Applies conditional logic to replace 'T' with 'U'
- Converts back to string format

## Test Example
Input: `"GATGGAACTTGACTACGTAAATT"`
Output: `"GAUGGAACUUGACUACGUAAAUU"`

The first approach is most straightforward and readable, while the second is the most concise if the language supports string replacement functions.

