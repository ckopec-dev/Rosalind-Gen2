# Rosalind Problem: Enumerating Unrooted Binary Trees

## Problem Statement
Given an integer n, return the number of distinct unrooted binary trees that can be formed with n leaves.

For an unrooted binary tree with n leaves, the number of such trees is given by the formula:
**(2n-5)!!** where !! denotes the double factorial.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Enumerating_Unrooted_Binary_Trees is
   
   -- Function to calculate double factorial
   function Double_Factorial(n : Integer) return Long_Long_Integer is
      result : Long_Long_Integer := 1;
   begin
      if n <= 0 then
         return 1;
      elsif n = 1 then
         return 1;
      else
         for i in reverse 1..n loop
            if i mod 2 = 1 then  -- Only odd numbers
               result := result * Long_Long_Integer(i);
            end if;
         end loop;
         return result;
      end if;
   end Double_Factorial;
   
   -- Alternative implementation using recursion
   function Double_Factorial_Recursive(n : Integer) return Long_Long_Integer is
   begin
      if n <= 1 then
         return 1;
      else
         return Long_Long_Integer(n) * Double_Factorial_Recursive(n - 2);
      end if;
   end Double_Factorial_Recursive;
   
   -- Main function to calculate number of unrooted binary trees
   function Count_Unrooted_Binary_Trees(n : Integer) return Long_Long_Integer is
   begin
      if n < 3 then
         return 0;  -- No unrooted binary trees with less than 3 leaves
      else
         -- For n leaves, number of unrooted binary trees = (2n-5)!!
         return Double_Factorial_Recursive(2 * n - 5);
      end if;
   end Count_Unrooted_Binary_Trees;
   
   -- Read input and process
   n : Integer;
   
begin
   Put("Enter number of leaves (n): ");
   Get(n);
   
   if n < 3 then
      Put_Line("Number of unrooted binary trees: 0");
   else
      declare
         result : Long_Long_Integer := Count_Unrooted_Binary_Trees(n);
      begin
         Put("Number of unrooted binary trees with ");
         Put(n);
         Put_Line(" leaves: ");
         Put(result);
         New_Line;
      end;
   end if;
   
end Enumerating_Unrooted_Binary_Trees;
```

## Explanation

The solution implements the mathematical formula for counting unrooted binary trees:

1. **Formula**: For n leaves, the number of distinct unrooted binary trees is (2n-5)!!
2. **Double Factorial**: The double factorial n!! means n × (n-2) × (n-4) × ... until reaching 1 or 2
3. **Edge Cases**: 
   - For n < 3, there are no valid unrooted binary trees
   - For n = 3, the answer is 1 tree
   - For n = 4, the answer is 3 trees

## Example Usage

For input n = 4:
- (2×4-5)!! = 3!! = 3 × 1 = 3

For input n = 5:
- (2×5-5)!! = 5!! = 5 × 3 × 1 = 15

## Test Cases

```
Input: 3
Output: 1

Input: 4  
Output: 3

Input: 5
Output: 15

Input: 6
Output: 105
```

The algorithm uses a recursive implementation of double factorial for clarity and correctness, though an iterative version could also be implemented for better performance with large inputs.