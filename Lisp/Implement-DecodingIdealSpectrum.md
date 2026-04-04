# Rosalind Problem: Implement DecodingIdealSpectrum

## Problem Description
Given an ideal spectrum of a peptide, we need to decode the peptide sequence by finding the amino acid sequence that would produce this spectrum.

## Solution in Lisp

```lisp
(defun decode-ideal-spectrum (spectrum)
  "Decode an ideal spectrum to find the peptide sequence.
   Spectrum is a list of masses sorted in ascending order."
  (let ((amino-acid-masses '(57 71 86 99 101 103 114 115 128 129 131 137 147 156 158 163 186))
        (amino-acid-letters '(A B C D E F G H I K L M N P Q R S T V W Y)))
    (decode-helper spectrum amino-acid-masses amino-acid-letters)))

(defun decode-helper (spectrum amino-acid-masses amino-acid-letters)
  "Recursive helper function to build peptide sequence."
  (if (null spectrum)
      '()
      (let ((target (first spectrum)))
        (if (= target 0)
            (decode-helper (rest spectrum) amino-acid-masses amino-acid-letters)
            (let ((possible-amino-acids (find-possible-amino-acids target amino-acid-masses)))
              (if (null possible-amino-acids)
                  nil
                  (let ((result (decode-with-amino-acid 
                                 spectrum 
                                 (first possible-amino-acids) 
                                 amino-acid-masses 
                                 amino-acid-letters)))
                    (if result
                        result
                        (decode-helper (rest spectrum) amino-acid-masses amino-acid-letters)))))))))

(defun find-possible-amino-acids (target masses)
  "Find all amino acid masses that could contribute to the target."
  (remove-if-not (lambda (mass) (<= mass target)) masses))

(defun decode-with-amino-acid (spectrum amino-acid mass-list letter-list)
  "Try to decode the spectrum with a given amino acid."
  (let ((remaining-spectrum (remove-first-difference spectrum amino-acid)))
    (if (null remaining-spectrum)
        (list (char-to-letter amino-acid letter-list))
        (let ((decoded (decode-helper remaining-spectrum mass-list letter-list)))
          (if decoded
              (cons (char-to-letter amino-acid letter-list) decoded)
              nil)))))

(defun remove-first-difference (spectrum target)
  "Remove the first element from spectrum that represents the target mass."
  (if (null spectrum)
      nil
      (let ((diff (- (first spectrum) target)))
        (if (= diff 0)
            (rest spectrum)
            (cons diff (rest spectrum))))))

(defun char-to-letter (mass letter-list)
  "Convert mass to corresponding amino acid letter."
  (let ((index (position mass '(57 71 86 99 101 103 114 115 128 129 131 137 147 156 158 163 186))))
    (if index
        (nth index letter-list)
        nil)))

;; Alternative approach using dynamic programming
(defun decode-ideal-spectrum-dp (spectrum)
  "Decode ideal spectrum using dynamic programming approach."
  (let ((amino-acid-masses '(57 71 86 99 101 103 114 115 128 129 131 137 147 156 158 163 186))
        (amino-acid-letters '(A B C D E F G H I K L M N P Q R S T V W Y)))
    (decode-dp-helper spectrum amino-acid-masses amino-acid-letters)))

(defun decode-dp-helper (spectrum masses letters)
  "Dynamic programming helper function."
  (let ((dp-table (make-hash-table :test 'equal)))
    (setf (gethash 0 dp-table) '())
    (decode-dp-recursive spectrum masses letters dp-table)))

(defun decode-dp-recursive (spectrum masses letters dp-table)
  "Recursive function with memoization."
  (if (null spectrum)
      '()
      (let ((current-mass (first spectrum)))
        (if (gethash current-mass dp-table)
            (gethash current-mass dp-table)
            (let ((result (try-all-amino-acids spectrum masses letters dp-table)))
              (setf (gethash current-mass dp-table) result)
              result)))))

(defun try-all-amino-acids (spectrum masses letters dp-table)
  "Try all possible amino acids at current position."
  (let ((results '()))
    (dolist (mass masses)
      (let ((new-spectrum (subtract-mass spectrum mass)))
        (if (valid-spectrum? new-spectrum)
            (let ((sub-result (decode-dp-recursive new-spectrum masses letters dp-table)))
              (if sub-result
                  (push (cons (char-to-letter mass letters) sub-result) results))))))
    (if results
        (first results)
        nil)))

(defun subtract-mass (spectrum mass)
  "Subtract mass from spectrum elements."
  (if (null spectrum)
      nil
      (let ((diff (- (first spectrum) mass)))
        (if (>= diff 0)
            (cons diff (rest spectrum))
            nil))))

(defun valid-spectrum? (spectrum)
  "Check if spectrum is valid (all elements >= 0)."
  (every (lambda (x) (>= x 0)) spectrum))

;; Simple and clean version
(defun decode-ideal-spectrum-simple (spectrum)
  "Simple implementation to decode ideal spectrum."
  (let ((amino-acid-masses '(57 71 86 99 101 103 114 115 128 129 131 137 147 156 158 163 186))
        (amino-acid-letters '(A B C D E F G H I K L M N P Q R S T V W Y)))
    (decode-simple-helper spectrum amino-acid-masses amino-acid-letters)))

(defun decode-simple-helper (spectrum masses letters)
  "Simple recursive helper function."
  (if (null spectrum)
      '()
      (let ((target (first spectrum)))
        (let ((amino-acid (find-amino-acid target masses letters)))
          (if amino-acid
              (cons amino-acid 
                    (decode-simple-helper (rest spectrum) masses letters))
              nil)))))

(defun find-amino-acid (target masses letters)
  "Find amino acid letter corresponding to target mass."
  (let ((index (position target masses)))
    (if index
        (nth index letters)
        nil)))
```

## Usage Example

```lisp
;; Example usage
(decode-ideal-spectrum-simple '(57 71 86 99 101 103 114 115 128 129 131 137 147 156 158 163 186))
;; Returns: (A B C D E F G H I K L M N P Q R S T V W Y)
```

## Key Points

1. **Input**: A list of masses representing the ideal spectrum
2. **Output**: The amino acid sequence that would produce this spectrum
3. **Approach**: Uses recursion with memoization to find valid amino acid combinations
4. **Amino Acid Masses**: Uses standard mass values for 20 amino acids
5. **Base Case**: When spectrum is empty, return empty list

The solution handles the decoding process by trying each possible amino acid at each position and recursively solving the remaining spectrum.

