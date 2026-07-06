# Rosalind Problem: Fibonacci Numbers - Ada Solution

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of Fn, where Fn is the nth Fibonacci number.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Fibonacci_Numbers is
   n : Integer;
   fn : Integer := 0;
   fn_minus_1 : Integer := 1;
   fn_minus_2 : Integer := 0;
begin
   -- Read input
   Put_Line("Enter a positive integer n (≤ 25): ");
   Get(n);
   
   -- Handle base cases
   if n = 0 then
      fn := 0;
   elsif n = 1 then
      fn := 1;
   else
      -- Calculate Fibonacci number iteratively
      for i in 2..n loop
         fn := fn_minus_1 + fn_minus_2;
         fn_minus_2 := fn_minus_1;
         fn_minus_1 := fn;
      end loop;
   end if;
   
   -- Output result
   Put_Line("F" & Integer'Image(n) & " = " & Integer'Image(fn));
end Fibonacci_Numbers;
```

## Alternative Recursive Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;

function Fibonacci(n : Integer) return Integer is
begin
   if n = 0 then
      return 0;
   elsif n = 1 then
      return 1;
   else
      return Fibonacci(n - 1) + Fibonacci(n - 2);
   end if;
end Fibonacci;

procedure Fibonacci_Numbers is
   n : Integer;
   result : Integer;
begin
   Put_Line("Enter a positive integer n (≤ 25): ");
   Get(n);
   
   result := Fibonacci(n);
   
   Put_Line("F" & Integer'Image(n) & " = " & Integer'Image(result));
end Fibonacci_Numbers;
```

## Explanation

The problem asks us to compute the nth Fibonacci number where F₀ = 0, F₁ = 1, and Fₙ = Fₙ₋₁ + Fₙ₋₂ for n > 1.

**Iterative Approach:**
- Uses three variables to keep track of consecutive Fibonacci numbers
- More efficient with O(n) time complexity and O(1) space complexity
- Avoids the exponential time complexity of naive recursion

**Recursive Approach:**
- Direct implementation of the mathematical definition
- Simpler code but much less efficient for large n due to repeated calculations
- Time complexity is O(2ⁿ) - not recommended for larger inputs

## Sample Input/Output
```
Enter a positive integer n (≤ 25): 
10
F 10 = 55
```

The iterative solution is preferred for its efficiency and suitability for the given constraints (n ≤ 25).