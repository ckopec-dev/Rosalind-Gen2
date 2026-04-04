# Rosalind Problem: Independent Alleles (Independent Alleles)

## Problem Statement
In a monohybrid cross, we have two alleles (A and a) where A is dominant and a is recessive. If we have two heterozygous individuals (Aa × Aa), the probability of getting a recessive phenotype (aa) is 1/4.

For n generations of backcrossing, we want to find the probability of having at least k AaBb organisms in the n-th generation, where each organism has two independent gene pairs.

## Solution Approach

This is a binomial probability problem. Each organism in generation n has 2^n offspring, and each offspring has probability (1/4) of being AaBb (since each gene pair independently follows Mendel's laws).

The probability of having exactly i AaBb organisms out of 2^n total organisms follows a binomial distribution:
P(X = i) = C(2^n, i) × (1/4)^i × (3/4)^(2^n - i)

We want P(X ≥ k) = 1 - P(X < k) = 1 - Σ(i=0 to k-1) P(X = i)

## Haskell Implementation

```haskell
import Data.List (foldl')

-- Calculate combination C(n,k)
comb :: Integer -> Integer -> Integer
comb n k
    | k > n = 0
    | k == 0 || k == n = 1
    | k > n `div` 2 = comb n (n - k)
    | otherwise = comb n (k - 1) * (n - k + 1) `div` k

-- Calculate binomial probability P(X = i)
binomialProb :: Integer -> Integer -> Double -> Double
binomialProb n i p = fromInteger (comb n i) * (p ^^ i) * ((1 - p) ^^ (n - i))

-- Calculate probability of at least k successes in n trials with probability p
probAtLeastK :: Integer -> Integer -> Double -> Double
probAtLeastK n k p = 1 - sum [binomialProb n i p | i <- [0..k-1]]

-- Main function for the problem
independentAlleles :: Integer -> Integer -> Double
independentAlleles n k = probAtLeastK (2^n) k 0.25

-- Alternative implementation using more efficient computation
independentAlleles' :: Integer -> Integer -> Double
independentAlleles' n k = 1 - cumulativeProb (2^n) k 0.25
  where
    cumulativeProb n k p = sum [binomialProb n i p | i <- [0..k-1]]

-- Example usage
main :: IO ()
main = do
    -- Example: n=2, k=1
    let result = independentAlleles 2 1
    putStrLn $ "Probability of at least 1 AaBb organism in generation 2: " ++ show result
    
    -- Another example: n=3, k=5
    let result2 = independentAlleles 3 5
    putStrLn $ "Probability of at least 5 AaBb organisms in generation 3: " ++ show result2
```

## Explanation

1. **Combination Function**: `comb n k` calculates C(n,k) using an efficient approach that takes advantage of the symmetry property.

2. **Binomial Probability**: `binomialProb n i p` computes the probability of exactly i successes in n trials with success probability p.

3. **Main Function**: `independentAlleles n k` calculates the probability of having at least k AaBb organisms in generation n.

4. **Key Insight**: In generation n, there are 2^n offspring total, and each has probability 0.25 of being AaBb.

## Example
For n=2, k=1:
- Generation 2 has 2^2 = 4 offspring
- Probability of at least 1 AaBb = 1 - P(0 AaBb) = 1 - (3/4)^4 = 1 - 81/256 = 175/256

The solution handles large numbers efficiently and provides the exact probability as requested in the Rosalind problem.

