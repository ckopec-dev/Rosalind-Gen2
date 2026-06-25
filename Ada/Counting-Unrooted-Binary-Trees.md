# Rosalind Problem: Counting Unrooted Binary Trees

## Problem Statement
Given a collection of n taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

For n taxa, the number of distinct unrooted binary trees is given by the formula:
**(2n - 5)!!** (double factorial)

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Counting_Unrooted_Binary_Trees is
   
   -- Function to compute double factorial
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
   
   -- Alternative implementation using recursion for double factorial
   function Double_Fact(n : Integer) return Long_Long_Integer is
   begin
      if n <= 1 then
         return 1;
      else
         return Long_Long_Integer(n) * Double_Fact(n - 2);
      end if;
   end Double_Fact;
   
   -- Main computation function
   function Count_Unrooted_Binary_Trees(n : Integer) return Long_Long_Integer is
   begin
      if n < 3 then
         return 1;  -- For n=1 or n=2, there's only one tree
      else
         -- Formula for unrooted binary trees: (2n-5)!!
         return Double_Fact(2 * n - 5);
      end if;
   end Count_Unrooted_Binary_Trees;
   
   -- Read input and solve
   n : Integer;
begin
   Put_Line("Enter number of taxa:");
   Get(n);
   
   if n < 3 then
      Put_Line("Number of unrooted binary trees: 1");
   else
      declare
         result : Long_Long_Integer := Count_Unrooted_Binary_Trees(n);
      begin
         Put("Number of unrooted binary trees with ");
         Put(n);
         Put_Line(" taxa:");
         Put(result);
         New_Line;
      end;
   end if;
   
end Counting_Unrooted_Binary_Trees;
```

## Explanation

1. **Double Factorial Function**: The solution uses a double factorial function to compute (2n-5)!!. 
   - For odd numbers, it multiplies all odd numbers from n down to 1
   - For example: 5!! = 5 × 3 × 1 = 15

2. **Formula Application**: For n taxa, the number of distinct unrooted binary trees is given by (2n-5)!!

3. **Special Cases**: 
   - For n < 3, there's only one possible tree structure
   - For n = 3, there's 1 tree: 3!! = 3 × 1 = 3 (but the formula gives 1)
   - Actually, for n=3, the correct count is 1 (the double factorial approach needs to be corrected)

## Corrected Version

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Counting_Unrooted_Binary_Trees is
   
   -- Function to compute double factorial recursively
   function Double_Factorial(n : Integer) return Long_Long_Integer is
   begin
      if n <= 1 then
         return 1;
      else
         return Long_Long_Integer(n) * Double_Factorial(n - 2);
      end if;
   end Double_Factorial;
   
   -- For n taxa, the number of unrooted binary trees = (2n-5)!!
   function Count_Unrooted_Binary_Trees(n : Integer) return Long_Long_Integer is
   begin
      if n < 3 then
         return 1;
      else
         return Double_Factorial(2 * n - 5);
      end if;
   end Count_Unrooted_Binary_Trees;
   
   -- Main program
   n : Integer;
begin
   Put_Line("Rosalind: Counting Unrooted Binary Trees");
   Put_Line("Enter number of taxa (n):");
   Get(n);
   
   if n < 3 then
      Put_Line("Number of unrooted binary trees: 1");
   else
      declare
         result : Long_Long_Integer := Count_Unrooted_Binary_Trees(n);
      begin
         Put("Number of unrooted binary trees with ");
         Put(n);
         Put_Line(" taxa:");
         Put(result);
         New_Line;
      end;
   end if;
   
end Counting_Unrooted_Binary_Trees;
```

## Sample Input/Output

**Input:**
```
Enter number of taxa (n):
4
```

**Output:**
```
Number of unrooted binary trees with 4 taxa:
3
```

## Mathematical Background

For n taxa, the number of distinct unrooted binary trees follows the recurrence relation and can be computed as:
- For n = 1 or 2: 1 tree
- For n ≥ 3: (2n - 5)!! = (2n-5) × (2n-7) × ... × 3 × 1

This is a well-known result in combinatorial mathematics related to phylogenetic tree enumeration.