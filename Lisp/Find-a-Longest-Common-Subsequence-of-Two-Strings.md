# Find a Longest Common Subsequence of Two Strings - Lisp Solution

## Problem Understanding

We need to find the longest common subsequence (LCS) between two strings. A subsequence is a sequence that appears in the same relative order, but not necessarily contiguous.

## Approach

I'll use dynamic programming to solve this problem:
1. Create a 2D table where `dp[i][j]` represents the length of LCS for first `i` characters of string1 and first `j` characters of string2
2. Fill the table using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If they don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. Backtrack through the table to reconstruct the actual LCS

## Solution

```lisp
(defun longest-common-subsequence (str1 str2)
  "Find the longest common subsequence of two strings."
  (let* ((m (length str1))
         (n (length str2))
         ;; Create DP table
         (dp (make-array (list (1+ m) (1+ n)) :element-type 'fixnum :initial-element 0)))
    
    ;; Fill the DP table
    (loop for i from 1 to m do
      (loop for j from 1 to n do
        (if (char= (char str1 (1- i)) (char str2 (1- j)))
            (setf (aref dp i j) (1+ (aref dp (1- i) (1- j))))
            (setf (aref dp i j) (max (aref dp (1- i) j) (aref dp i (1- j)))))))
    
    ;; Backtrack to find the actual LCS
    (let ((lcs '()))
      (let loop ((i m) (j n))
        (cond
          ((or (= i 0) (= j 0)) 
           (coerce (nreverse lcs) 'string))
          ((char= (char str1 (1- i)) (char str2 (1- j)))
           (push (char str1 (1- i)) lcs)
           (loop (1- i) (1- j)))
          ((> (aref dp (1- i) j) (aref dp i (1- j)))
           (loop (1- i) j))
          (t
           (loop i (1- j))))))))

;; Alternative implementation with clearer variable names
(defun find-lcs (s1 s2)
  "Find longest common subsequence using dynamic programming."
  (let* ((len1 (length s1))
         (len2 (length s2))
         (table (make-array (list (1+ len1) (1+ len2)) 
                           :element-type 'fixnum 
                           :initial-element 0)))
    
    ;; Fill the table
    (loop for i from 1 to len1 do
      (loop for j from 1 to len2 do
        (if (char= (char s1 (1- i)) (char s2 (1- j)))
            (setf (aref table i j) (1+ (aref table (1- i) (1- j))))
            (setf (aref table i j) 
                  (max (aref table (1- i) j) 
                       (aref table i (1- j)))))))
    
    ;; Reconstruct the LCS
    (let ((result '()))
      (let backtrack ((i len1) (j len2))
        (cond
          ((or (= i 0) (= j 0)) 
           (coerce (nreverse result) 'string))
          ((char= (char s1 (1- i)) (char s2 (1- j)))
           (push (char s1 (1- i)) result)
           (backtrack (1- i) (1- j)))
          ((> (aref table (1- i) j) (aref table i (1- j)))
           (backtrack (1- i) j))
          (t
           (backtrack i (1- j))))))))

;; Example usage:
;; (find-lcs "ACGTACGT" "TACGTACG")
;; Expected output: "ACGTACG"

;; Test with Rosalind example:
;; (find-lcs "AACCTTGG" "ACACTGTGA")
;; Should return one of the possible LCS like "ACCTGG" or "ACGTGG"
```

## Explanation

1. **DP Table Construction**: 
   - Create a 2D array `dp` of size `(m+1) × (n+1)` where `m` and `n` are lengths of the input strings
   - Initialize all elements to 0

2. **Filling the Table**:
   - If characters match at positions `i-1` and `j-1`: `dp[i][j] = dp[i-1][j-1] + 1`
   - If they don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Backtracking**:
   - Start from `dp[m][n]` and work backwards
   - If characters match, include the character in result and move diagonally
   - Otherwise, move in the direction of the larger value

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Input/Output

For input strings "AACCTTGG" and "ACACTGTGA":
- The LCS could be "ACCTGG" or "ACGTGG" (both have length 6)

This solution efficiently finds the longest common subsequence using dynamic programming principles.

