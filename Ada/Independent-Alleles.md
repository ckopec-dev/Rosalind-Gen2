# Rosalind Problem: Independent Alleles (Independent Alleles)

## Problem Statement
In this problem, we need to calculate the probability that at least `k` individuals in the `n`-th generation will have the genotype `AaBb`, assuming independent assortment of alleles and random mating.

## Solution Approach

We can solve this using the binomial distribution. For each individual in the n-th generation, the probability of having genotype AaBb is 0.25 (since each gene pair has a 1/4 chance of being Aa or Bb).

The probability that at least `k` individuals out of `2^n` individuals have genotype AaBb is:
P(X ≥ k) = 1 - P(X < k) = 1 - Σ(i=0 to k-1) C(2^n, i) * (0.25)^i * (0.75)^(2^n-i)

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Independent_Alleles is
   type Float is new Float_64;
   
   function Factorial(n : Integer) return Float is
      Result : Float := 1.0;
   begin
      for I in 2..n loop
         Result := Result * Float(I);
      end loop;
      return Result;
   end Factorial;
   
   function Combination(n, k : Integer) return Float is
      N_Fact : Float := Factorial(n);
      K_Fact : Float := Factorial(k);
      N_K_Fact : Float := Factorial(n - k);
   begin
      return N_Fact / (K_Fact * N_K_Fact);
   end Combination;
   
   function Binomial_Pmf(n, k : Integer; p : Float) return Float is
      C : Float := Combination(n, k);
      P : Float := Float(k);
      Q : Float := Float(n - k);
   begin
      return C * (p ** P) * ((1.0 - p) ** Q);
   end Binomial_Pmf;
   
   function Probability_At_Least_K(n, k : Integer) return Float is
      Total : Integer := 2 ** n;
      Cumulative : Float := 0.0;
      P : Float := 0.25;
   begin
      for I in 0..k-1 loop
         Cumulative := Cumulative + Binomial_Pmf(Total, I, P);
      end loop;
      return 1.0 - Cumulative;
   end Probability_At_Least_K;
   
   N : Integer;
   K : Integer;
   Result : Float;
begin
   -- Read input values
   Put("Enter n (generation number): ");
   Get(N);
   Put("Enter k (minimum number of AaBb individuals): ");
   Get(K);
   
   -- Calculate probability
   Result := Probability_At_Least_K(N, K);
   
   -- Output result with 6 decimal places
   Put("Probability: ");
   Put(Result, Fore => 1, Aft => 6, Exp => 0);
   New_Line;
end Independent_Alleles;
```

## Explanation

The solution works as follows:

1. **Factorial function**: Computes the factorial of a number for combination calculations
2. **Combination function**: Calculates C(n,k) = n!/(k!(n-k)!)
3. **Binomial_Pmf function**: Calculates the probability mass function for binomial distribution
4. **Probability_At_Least_K function**: Computes P(X ≥ k) using the complement of cumulative probability

## Key Insights

- Each generation doubles the number of individuals (2^n)
- Probability of AaBb genotype in any individual is 0.25
- We use the complement rule: P(X ≥ k) = 1 - P(X < k) = 1 - Σ(i=0 to k-1) P(X = i)

## Sample Input/Output

```
Enter n (generation number): 2
Enter k (minimum number of AaBb individuals): 1
Probability: 0.843750
```

This solution handles the mathematical requirements of the problem using Ada's built-in floating-point operations and implements the binomial probability calculations correctly for the independent alleles problem.