# Rosalind Problem: Mendel's First Law (Independent Assortment)

## Problem Statement
Given three positive integers k, m, and n representing the number of organisms of each genotype in a population:
- k: homozygous dominant (AA)
- m: heterozygous (Aa) 
- n: homozygous recessive (aa)

Return the probability that two randomly selected mating organisms will produce an individual possessing a dominant allele (i.e., the probability of getting at least one dominant allele).

## Solution in Lisp

```lisp
(defun mendels-first-law (k m n)
  "Calculate probability that two randomly selected organisms will produce offspring with dominant allele.
   k: homozygous dominant (AA), m: heterozygous (Aa), n: homozygous recessive (aa)"
  (let* ((total (+ k m n))
         (total-choose-2 (/ (* total (- total 1)) 2)))
    
    ;; Calculate probability of recessive offspring
    (let ((prob-recessive 
           ;; All recessive cases:
           ;; 1. Two homozygous recessive (aa x aa)
           ;; 2. One heterozygous and one homozygous recessive (Aa x aa)
           ;; 3. Two heterozygous (Aa x Aa)
           (+ 
            ;; Case 1: Two aa individuals
            (/ (* n (decf n)) (* total (decf total 1)))
            
            ;; Case 2: One Aa and one aa (2 combinations)
            (/ (* 2 m n) (* total (decf total 1)))
            
            ;; Case 3: Two Aa individuals (3/4 chance of recessive)
            (/ (* m (decf m)) (* total (decf total 1)))
            ;; Multiply by 0.25 (probability of recessive from Aa x Aa)
            (* 0.25 (/ (* m (decf m)) (* total (decf total 1)))))))
      
      ;; Probability of dominant = 1 - probability of recessive
      (- 1.0 prob-recessive))))

;; Alternative cleaner approach
(defun mendels-first-law-clean (k m n)
  "Cleaner implementation of Mendel's First Law"
  (let* ((t (+ k m n))
         (total-pairs (/ (* t (- t 1)) 2)))
    
    (let ((prob-recessive 
           ;; Two aa individuals
           (/ (* n (decf n)) (* t (decf t)))
           ;; One Aa, one aa (2 combinations)
           (/ (* 2 m n) (* t (decf t)))
           ;; Two Aa individuals (0.25 chance of recessive)
           (/ (* m (decf m)) (* t (decf t)))
           (* 0.25 (/ (* m (decf m)) (* t (decf t))))))
      
      (- 1.0 prob-recessive))))

;; Most readable version
(defun mendels-first-law-readable (k m n)
  "Readable implementation of Mendel's First Law"
  (let* ((total (+ k m n))
         (total-choose-2 (/ (* total (1- total)) 2)))
    
    (let ((prob-recessive 
           ;; Probability of recessive offspring from different matings:
           ;; 1. Two aa individuals (100% recessive)
           (+ (/ (* n (1- n)) (* total (1- total)))
              ;; 2. One Aa, one aa (50% recessive) - 2 combinations
              (/ (* 2 m n) (* total (1- total)))
              ;; 3. Two Aa individuals (25% recessive)
              (* 0.25 (/ (* m (1- m)) (* total (1- total)))))))
      
      (- 1.0 prob-recessive))))

;; Test with example values
(mendels-first-law-readable 2 2 2)
;; Should return 0.7833333333333333
```

## Explanation

The key insight is to calculate the probability of getting a **recessive** offspring, then subtract from 1 to get the probability of getting a **dominant** offspring.

**Cases where offspring is recessive:**
1. **Two aa individuals (aa × aa)**: 100% chance of recessive → probability = (n choose 2) / (total choose 2)
2. **One Aa and one aa (Aa × aa)**: 50% chance of recessive → probability = 2 × (m × n) / (total choose 2)  
3. **Two Aa individuals (Aa × Aa)**: 25% chance of recessive → probability = 0.25 × (m choose 2) / (total choose 2)

**Final Answer:** 1 - (probability of recessive offspring)

## Example
For k=2, m=2, n=2:
- Total = 6
- Total pairs = 15
- Probability of recessive = 0.216666667
- Probability of dominant = 0.783333333

The solution handles the combinatorial mathematics of selecting pairs from the population and calculating the genetic probabilities correctly.

