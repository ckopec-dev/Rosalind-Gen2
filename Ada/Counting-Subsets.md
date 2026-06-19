# Rosalind Problem: Counting Subsets

## Problem Statement
Given a positive integer n ≤ 1000, return the number of all possible subsets of {1,2,…,n}.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Counting_Subsets is
   n : Integer;
   result : Long_Long_Integer;
begin
   -- Read input
   Get(n);
   
   -- For a set with n elements, the number of subsets is 2^n
   -- We need to compute 2^n mod (10^6 + 3) as the answer
   result := 1;
   for i in 1..n loop
      result := (result * 2) mod 1000003;
   end loop;
   
   Put(result);
   New_Line;
end Counting_Subsets;
```

## Explanation

The mathematical principle behind this problem is that for a set with n elements, the total number of subsets is 2^n. This includes:
- The empty set
- All single-element subsets
- All two-element subsets
- And so on...

For example:
- If n = 3, we have {1,2,3} and the subsets are: ∅, {1}, {2}, {3}, {1,2}, {1,3}, {2,3}, {1,2,3} = 8 subsets
- This equals 2^3 = 8

Since the result can be very large, we compute it modulo 10^6 + 3 (as is common in competitive programming problems) to prevent integer overflow.

The algorithm:
1. Read the input integer n
2. Compute 2^n mod 1000003 using modular exponentiation
3. Output the result

## Sample Input/Output
```
Input: 3
Output: 8

Input: 10
Output: 1024
```