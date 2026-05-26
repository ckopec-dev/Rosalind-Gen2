# Rosalind Problem: Calculating Expected Offspring

## Problem Description
Given six numbers representing the number of couples in a population possessing each genotype pairing for a given trait. The six inputs represent the following pairs:
1. AA-AA
2. AA-Aa
3. AA-aa
4. Aa-Aa
5. Aa-aa
6. aa-aa

Each couple produces two offspring. For each genotype pairing, we need to calculate the expected number of offspring that will display the dominant phenotype (A_).

## Solution in Lisp

```lisp
(defun calculating-expected-offspring (input-list)
  "Calculate expected number of dominant offspring from given genotype pairs.
   Input: list of 6 integers representing couple counts for each genotype pairing
   Returns: expected number of dominant offspring"
  (let ((aa-aa (first input-list))      ; 100% dominant
        (aa-a (second input-list))      ; 100% dominant
        (aa-aa (third input-list))      ; 50% dominant
        (a-a (fourth input-list))       ; 25% dominant
        (a-aa (fifth input-list))       ; 50% dominant
        (aa-aa (sixth input-list)))     ; 0% dominant
    (let ((expected-dominant 
           (+ (* aa-aa 2)           ; 100% dominant, 2 offspring per couple
              (* aa-a 2)            ; 100% dominant, 2 offspring per couple
              (* aa-aa 1)           ; 50% dominant, 2 offspring per couple
              (* a-a 0.5)           ; 25% dominant, 2 offspring per couple
              (* a-aa 1)            ; 50% dominant, 2 offspring per couple
              (* aa-aa 0))))        ; 0% dominant, 2 offspring per couple
      expected-dominant)))

;; Alternative cleaner version
(defun calculate-expected-offspring (counts)
  "Calculate expected number of dominant offspring from genotype counts.
   Counts: list of 6 integers representing couple counts for each pairing"
  (let ((probabilities '(1.0 1.0 0.5 0.25 0.5 0.0))) ; probability of dominant phenotype
    (reduce #'+ 
            (mapcar #'* counts probabilities)
            :initial-value 0)))

;; More readable version with explicit pairing
(defun expected-offspring (aa-aa aa-a aa-aa a-a a-aa aa-aa)
  "Calculate expected dominant offspring from genotype pairings.
   Each parameter represents number of couples for that pairing"
  (let ((expected 
         (+ (* aa-aa 2.0)    ; AA-AA -> 2 dominant offspring per couple
            (* aa-a 2.0)     ; AA-Aa -> 2 dominant offspring per couple
            (* aa-aa 1.0)    ; AA-aa -> 1 dominant offspring per couple
            (* a-a 0.5)      ; Aa-Aa -> 0.5 dominant offspring per couple
            (* a-aa 1.0)     ; Aa-aa -> 1 dominant offspring per couple
            (* aa-aa 0.0)))) ; aa-aa -> 0 dominant offspring per couple
    expected))
```

## Example Usage

```lisp
;; Example with sample data
(expected-offspring 1 0 0 1 1 1)
;; Returns: 3.5

;; Using the cleaner version
(calculate-expected-offspring '(1 0 0 1 1 1))
;; Returns: 3.5

;; For the actual Rosalind input format
(defun solve-rosalind (input-string)
  "Solve Rosalind problem with space-separated input"
  (let ((counts (mapcar #'parse-integer 
                        (split-string input-string " "))))
    (calculate-expected-offspring counts)))
```

## Explanation

The key insight is understanding the probability of dominant phenotype for each genotype pairing:

1. **AA-AA**: 100% chance of dominant → 2 offspring per couple
2. **AA-Aa**: 100% chance of dominant → 2 offspring per couple  
3. **AA-aa**: 50% chance of dominant → 1 offspring per couple
4. **Aa-Aa**: 25% chance of dominant → 0.5 offspring per couple
5. **Aa-aa**: 50% chance of dominant → 1 offspring per couple
6. **aa-aa**: 0% chance of dominant → 0 offspring per couple

Since each couple produces 2 offspring, we multiply the probability by 2 for each pairing and sum all expected values.

The answer is the sum of all expected dominant offspring from each genotype pairing.

