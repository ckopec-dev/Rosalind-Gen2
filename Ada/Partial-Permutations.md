# Rosalind Problem: Partial Permutations

## Problem Description
Partial permutations are an extension of permutations where we only consider arrangements of k objects chosen from n objects, rather than all n objects.

The number of partial permutations of k objects chosen from n objects is given by:
P(n,k) = n! / (n-k)! = n × (n-1) × (n-2) × ... × (n-k+1)

Since the result can be very large, we need to return the answer modulo 1,000,000.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Partial_Permutations is
   Modulo : constant := 1_000_000;
   
   function Partial_Permutation(n, k : Integer) return Integer is
      Result : Integer := 1;
      I : Integer;
   begin
      -- Calculate P(n,k) = n * (n-1) * ... * (n-k+1)
      for I in 1 .. k loop
         Result := (Result * (n - I + 1)) mod Modulo;
      end loop;
      
      return Result;
   end Partial_Permutation;
   
   N, K : Integer;
begin
   -- Read input values
   Get(N);
   Get(K);
   
   -- Calculate and output the partial permutation
   Put(Partial_Permutation(N, K));
   New_Line;
end Partial_Permutations;
```

## Explanation

1. **Modulo Operation**: Since the result can be very large, we use modulo 1,000,000 as required.

2. **Algorithm**: Instead of calculating factorials separately and then dividing, we directly compute the partial permutation by multiplying n × (n-1) × ... × (n-k+1).

3. **Efficiency**: The approach avoids computing large factorials and instead multiplies incrementally while taking modulo at each step to prevent overflow.

4. **Input/Output**: 
   - Reads two integers n and k from standard input
   - Outputs the result of P(n,k) modulo 1,000,000

## Example

For input:
```
21 7
```

The calculation would be:
P(21,7) = 21 × 20 × 19 × 18 × 17 × 16 × 15 = 51,003,600

Result modulo 1,000,000 = 3600

## Time Complexity
O(k) - we perform k multiplications and modulo operations.

## Space Complexity
O(1) - only using a constant amount of extra space.