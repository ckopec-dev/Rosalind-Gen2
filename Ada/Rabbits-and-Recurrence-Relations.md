# Rosalind Problem: Rabbits and Recurrence Relations (Ada Solution)

## Problem Statement
A recurrence relation is a method for defining sequences of numbers where each term is defined as a function of the preceding terms. In this problem, we need to compute the number of rabbit pairs after n months, given that:
- Each pair of rabbits produces a new pair every month
- Rabbits begin reproducing from the second month onward
- Each pair lives forever
- We start with 1 pair of rabbits

This is the classic Fibonacci sequence: F(n) = F(n-1) + F(n-2)

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Rabbits_and_Recurrence_Relations is
   n : Integer;
   fn, fn1, fn2 : Long_Long_Integer;
begin
   -- Read input
   Put("Enter number of months (n): ");
   Get(n);
   
   -- Handle base cases
   if n = 1 then
      Put_Line("Number of rabbit pairs: 1");
   elsif n = 2 then
      Put_Line("Number of rabbit pairs: 1");
   else
      -- Initialize first two values
      fn1 := 1;  -- F(1) = 1
      fn2 := 1;  -- F(2) = 1
      
      -- Compute F(n) using iterative approach
      for i in 3..n loop
         fn := fn1 + fn2;
         fn2 := fn1;
         fn1 := fn;
      end loop;
      
      Put_Line("Number of rabbit pairs: " & Integer'Image(Integer(fn)));
   end if;
end Rabbits_and_Recurrence_Relations;
```

## Alternative Solution (More Explicit)

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Rabbits_and_Recurrence_Relations is
   n : Integer;
   
   function Fibonacci(n : Integer) return Long_Long_Integer is
      fn, fn1, fn2 : Long_Long_Integer;
   begin
      if n = 1 or n = 2 then
         return 1;
      else
         fn1 := 1;
         fn2 := 1;
         
         for i in 3..n loop
            fn := fn1 + fn2;
            fn2 := fn1;
            fn1 := fn;
         end loop;
         
         return fn;
      end if;
   end Fibonacci;
   
begin
   -- Read input
   Put("Enter number of months (n): ");
   Get(n);
   
   -- Calculate and output result
   Put_Line("Number of rabbit pairs: " & Integer'Image(Integer(Fibonacci(n))));
end Rabbits_and_Recurrence_Relations;
```

## How it Works

1. **Input**: Read the number of months `n` from standard input
2. **Base Cases**: 
   - For n=1, return 1 pair (initial pair)
   - For n=2, return 1 pair (pair matures but doesn't reproduce yet)
3. **Iterative Calculation**: Use the recurrence relation F(n) = F(n-1) + F(n-2)
4. **Memory Efficient**: Only store the last two values to compute current value
5. **Output**: Display the final number of rabbit pairs

## Example Run

```
Enter number of months (n): 5
Number of rabbit pairs: 5
```

## Time and Space Complexity
- **Time Complexity**: O(n) - single loop through n months
- **Space Complexity**: O(1) - only storing a few variables regardless of input size

This solution efficiently computes the Fibonacci sequence using an iterative approach, avoiding the exponential time complexity of a naive recursive implementation.