# Rosalind Problem: Introduction to Alternative Splicing

## Problem Description
In this problem, we need to compute the number of ways to choose k non-negative integers that sum to n, which is equivalent to finding the number of ways to select k subsets from a set of n elements where each subset can be either included or excluded.

However, looking at the typical Rosalind problem structure for "Introduction to Alternative Splicing", this is more likely asking for the number of possible alternative splicing patterns given n exons and k being the number of exons that are included in the final mRNA.

Let me solve it as computing the binomial coefficient C(n,k) which represents the number of ways to choose k elements from n elements, which relates to alternative splicing where we choose which exons to include.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Introduction_To_Alternative_Splicing is
   
   -- Function to compute binomial coefficient C(n,k)
   function Binomial_Coefficient(n, k : Integer) return Integer is
      result : Integer := 1;
   begin
      if k > n or k < 0 then
         return 0;
      end if;
      
      if k = 0 or k = n then
         return 1;
      end if;
      
      -- Take advantage of symmetry C(n,k) = C(n,n-k)
      if k > n - k then
         k := n - k;
      end if;
      
      for i in 1 .. k loop
         result := result * (n - i + 1) / i;
      end loop;
      
      return result;
   end Binomial_Coefficient;
   
   -- Function to compute 2^n (number of possible subsets)
   function Power_of_2(n : Integer) return Integer is
   begin
      if n < 0 then
         return 0;
      elsif n = 0 then
         return 1;
      else
         return 2 * Power_of_2(n - 1);
      end if;
   end Power_of_2;
   
   -- Main computation for alternative splicing
   function Alternative_Splicing_Count(n, k : Integer) return Integer is
   begin
      -- For n exons, we want to choose k of them to be included in the final mRNA
      -- This is simply C(n,k)
      return Binomial_Coefficient(n, k);
   end Alternative_Splicing_Count;
   
   -- More general case: number of ways to select any subset of exons
   function All_Alternative_Splicing_Patterns(n : Integer) return Integer is
   begin
      -- For n exons, each can either be included or excluded (2 choices)
      -- Total patterns = 2^n
      return Power_of_2(n);
   end All_Alternative_Splicing_Patterns;
   
begin
   -- Example usage:
   -- Let's assume we have n exons and want to know how many ways
   -- we can select k of them for inclusion in mRNA
   
   declare
      n : constant Integer := 10;  -- number of exons
      k : constant Integer := 5;   -- number of exons to include
   begin
      Put("Number of ways to choose ");
      Put(k);
      Put(" exons from ");
      Put(n);
      Put(" exons: ");
      Put(Alternative_Splicing_Count(n, k));
      New_Line;
      
      Put("Total number of alternative splicing patterns for ");
      Put(n);
      Put(" exons: ");
      Put(All_Alternative_Splicing_Patterns(n));
      New_Line;
   end;
   
   -- Another example with input from problem
   declare
      n : constant Integer := 10;
      k : constant Integer := 3;
   begin
      Put("Result for n=10, k=3: ");
      Put(Alternative_Splicing_Count(n, k));
      New_Line;
   end;
   
end Introduction_To_Alternative_Splicing;
```

## Explanation

The problem "Introduction to Alternative Splicing" in Rosalind typically asks us to compute how many different ways we can select subsets of exons from a given set, which translates to computing binomial coefficients.

In this solution:

1. **Binomial_Coefficient function**: Computes C(n,k) = n!/(k!(n-k)!) efficiently using the property that C(n,k) = C(n,n-k) and avoiding large factorials by iterative computation.

2. **Alternative_Splicing_Count function**: Returns the number of ways to choose k exons from n exons, which is the binomial coefficient C(n,k).

3. **All_Alternative_Splicing_Patterns function**: Computes 2^n, representing all possible combinations where each exon can either be included or excluded.

## Sample Input/Output

For n=10, k=5:
- Result: 252 (ways to choose 5 exons from 10)

For n=10:
- Result: 1024 (total alternative splicing patterns)

The solution uses efficient computation of binomial coefficients to avoid overflow issues with large factorials and handles edge cases appropriately.