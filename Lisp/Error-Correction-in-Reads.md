# Rosalind Problem: Error Correction in Reads (Lisp Solution)

## Problem Understanding

The task is to identify and correct sequencing errors in DNA reads. Errors can occur as:
1. Single base substitutions
2. Single base insertions/deletions
3. Reverse complements of existing sequences

We need to:
- Find reads that appear only once (likely erroneous)
- Find reads that appear twice (one correct, one erroneous)
- Correct the erroneous reads by finding the correct version

## Solution Approach

```lisp
(defun reverse-complement (seq)
  "Return the reverse complement of a DNA sequence"
  (let ((complement '((#\A . #\T) (#\T . #\A) (#\C . #\G) (#\G . #\C))))
    (map 'string 
         (lambda (base)
           (cdr (assoc base complement)))
         (reverse seq))))


(defun hamming-distance (s1 s2)
  "Calculate Hamming distance between two strings of equal length"
  (if (or (null s1) (null s2))
      0
      (let ((distance 0))
        (loop for i from 0 below (length s1)
              do (if (not (char= (char s1 i) (char s2 i)))
                     (incf distance)))
        distance)))


(defun find-corrections (reads)
  "Find and correct errors in DNA reads"
  (let ((read-counts (make-hash-table :test 'equal))
        (corrections '()))
    
    ;; Count occurrences of each read
    (loop for read in reads
          do (let ((count (gethash read read-counts 0)))
               (setf (gethash read read-counts) (1+ count))))
    
    ;; Find reads that appear only once (potential errors)
    (let ((single-reads '())
          (double-reads '()))
      
      ;; Separate single and double occurrences
      (loop for read being the hash-keys of read-counts
            for count = (gethash read read-counts)
            do (if (= count 1)
                   (push read single-reads)
                   (push read double-reads)))
      
      ;; Process single reads to find corrections
      (loop for single-read in single-reads
            do (let ((rc (reverse-complement single-read))
                     (found-correction nil))
                 
                 ;; Check if reverse complement exists in double-reads
                 (loop for double-read in double-reads
                       do (if (or (string= single-read double-read)
                                  (string= rc double-read))
                              (progn
                                (push (format nil "~A->~A" single-read double-read)
                                      corrections)
                                (setf found-correction t)
                                (return))))
                 
                 ;; Check for single base differences
                 (unless found-correction
                   (loop for double-read in double-reads
                         do (if (and (= (length single-read) (length double-read))
                                     (= (hamming-distance single-read double-read) 1))
                                (progn
                                  (push (format nil "~A->~A" single-read double-read)
                                        corrections)
                                  (setf found-correction t)
                                  (return)))))))
    
    corrections))


;; Alternative approach for better performance
(defun error-correction (reads)
  "Correct errors in DNA reads using frequency analysis"
  (let ((read-freq (make-hash-table :test 'equal))
        (corrections '()))
    
    ;; Count all reads (including reverse complements)
    (loop for read in reads
          do (let ((rc (reverse-complement read)))
               (incf (gethash read read-freq 0))
               (incf (gethash rc read-freq 0))))
    
    ;; Find reads that appear only once (after including reverse complements)
    (let ((single-reads '()))
      (loop for read being the hash-keys of read-freq
            for count = (gethash read read-freq)
            do (if (= count 1)
                   (push read single-reads)))
      
      ;; For each single read, find its correction
      (loop for single-read in single-reads
            do (let ((rc (reverse-complement single-read))
                     (found-correction nil))
                 
                 ;; Look for exact matches in original reads
                 (loop for original-read in reads
                       do (if (and (not (string= single-read original-read))
                                   (or (string= single-read original-read)
                                       (string= rc original-read)))
                              (progn
                                (push (format nil "~A->~A" single-read original-read)
                                      corrections)
                                (setf found-correction t)
                                (return)))))
      
      ;; Also check for single base differences
      (loop for single-read in single-reads
            do (let ((found-correction nil))
                 (loop for original-read in reads
                       do (if (and (not (string= single-read original-read))
                                   (= (length single-read) (length original-read))
                                   (= (hamming-distance single-read original-read) 1))
                              (progn
                                (push (format nil "~A->~A" single-read original-read)
                                      corrections)
                                (setf found-correction t)
                                (return)))))))
    
    corrections))


;; More robust solution
(defun solve-error-correction (reads)
  "Main function to solve error correction problem"
  (let ((read-counts (make-hash-table :test 'equal))
        (corrections '())
        (valid-reads '()))
    
    ;; Count all reads
    (loop for read in reads
          do (incf (gethash read read-counts 0)))
    
    ;; Separate valid (appearing > 1 times) and invalid (appearing = 1 time) reads
    (let ((invalid-reads '())
          (valid-reads '()))
      
      (loop for read being the hash-keys of read-counts
            for count = (gethash read read-counts)
            do (if (= count 1)
                   (push read invalid-reads)
                   (push read valid-reads)))
      
      ;; For each invalid read, find its correction
      (loop for invalid-read in invalid-reads
            do (let ((rc (reverse-complement invalid-read))
                     (found-correction nil))
                 
                 ;; Check against valid reads
                 (loop for valid-read in valid-reads
                       do (if (or (string= invalid-read valid-read)
                                  (string= rc valid-read))
                              (progn
                                (push (format nil "~A->~A" invalid-read valid-read)
                                      corrections)
                                (setf found-correction t)
                                (return)))))
                 
                 ;; Check for single base differences
                 (unless found-correction
                   (loop for valid-read in valid-reads
                         do (if (and (= (length invalid-read) (length valid-read))
                                     (= (hamming-distance invalid-read valid-read) 1))
                                (progn
                                  (push (format nil "~A->~A" invalid-read valid-read)
                                        corrections)
                                  (setf found-correction t)
                                  (return)))))))
    
    corrections))


;; Example usage:
(defun example-solution ()
  "Example of how to use the solution"
  (let ((reads '("TCATC" "TCTGC" "TCATC" "TCATC" "TCTGC" "TCATC" "TCATC" "TCATC")))
    (solve-error-correction reads)))

;; Helper function to read FASTA format
(defun read-fasta-reads (filename)
  "Read DNA reads from FASTA file"
  (let ((reads '())
        (current-read '()))
    (with-open-file (stream filename :direction :input :if-does-not-exist :error)
      (loop for line = (read-line stream nil nil)
            while line
            do (if (char= (char line 0) #\>)
                   (progn
                     (when current-read
                       (push (coerce current-read 'string) reads))
                     (setf current-read '()))
                   (progn
                     (loop for char across line
                           do (push char current-read)))))
      (when current-read
        (push (coerce (reverse current-read) 'string) reads)))
    (reverse reads)))
```

## Key Functions Explained

1. **`reverse-complement`**: Computes the reverse complement of a DNA sequence
2. **`hamming-distance`**: Calculates the number of differences between two strings
3. **`solve-error-correction`**: Main algorithm that:
   - Counts frequency of each read
   - Identifies erroneous reads (appearing once)
   - Finds corrections by checking reverse complements and single-base differences
   - Returns correction pairs in the required format

## Time Complexity
- O(n × m) where n is the number of reads and m is the average read length
- Efficient for typical Rosalind problem sizes

## Usage Example
```lisp
;; Sample input
(setq reads '("TCATC" "TCTGC" "TCATC" "TCATC" "TCTGC" "TCATC"))
(solve-error-correction reads)
```

This solution handles the core requirements of error correction in DNA sequencing reads as specified in the Rosalind problem.

