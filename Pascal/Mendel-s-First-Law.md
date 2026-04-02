# Rosalind Problem: Mendel's First Law (Mendels_First_Law)

## Problem Description
Given three positive integers k, m, and n representing the number of organisms of each genotype in a population:
- k: homozygous dominant (AA)
- m: heterozygous (Aa) 
- n: homozygous recessive (aa)

Calculate the probability that two randomly selected organisms will produce an offspring with a dominant allele (A).

## Solution in Pascal

```pascal
program MendelsFirstLaw;
uses math;

var
  k, m, n: longint;
  total: real;
  probability: real;

begin
  // Read input values
  readln(k, m, n);
  
  // Calculate total number of organisms
  total := k + m + n;
  
  // Calculate probability of getting dominant phenotype
  // This is 1 minus the probability of getting recessive phenotype
  // Recessive offspring occurs when both parents are recessive (aa) or 
  // one parent is heterozygous (Aa) and the other is recessive (aa)
  
  // Probability of two recessive parents (aa x aa)
  // P(aa x aa) = (n/total) * ((n-1)/(total-1))
  // This gives probability of recessive offspring from two recessive parents
  
  // Probability of one heterozygous and one recessive parent (Aa x aa)
  // P(Aa x aa) = (m/total) * (n/(total-1)) + (n/total) * (m/(total-1))
  // This gives probability of recessive offspring from heterozygous x recessive
  
  // Probability of two heterozygous parents (Aa x Aa)
  // P(Aa x Aa) = (m/total) * ((m-1)/(total-1))
  // This gives probability of recessive offspring from two heterozygous parents
  
  // Probability of recessive offspring = P(aa x aa) + P(Aa x aa) + P(Aa x Aa) * 0.25
  // (because 25% of Aa x Aa offspring are recessive)
  
  // Actually, let's calculate it more systematically:
  // Probability of recessive offspring = 
  // P(aa x aa) + P(Aa x aa) * 0.5 + P(Aa x Aa) * 0.25
  
  // P(aa x aa) = (n/total) * ((n-1)/(total-1))
  // P(Aa x aa) = 2 * (m/total) * (n/(total-1))  (since order doesn't matter)
  // P(Aa x Aa) = (m/total) * ((m-1)/(total-1))
  
  // P(recessive) = (n/total) * ((n-1)/(total-1)) + 2 * (m/total) * (n/(total-1)) * 0.5 + 
  //                 (m/total) * ((m-1)/(total-1)) * 0.25
  
  // Simplifying:
  // P(recessive) = (n*(n-1))/(total*(total-1)) + (m*n)/(total*(total-1)) + (m*(m-1)*0.25)/(total*(total-1))
  
  // P(dominant) = 1 - P(recessive)
  
  probability := 1.0 - 
    ((n * (n - 1)) / (total * (total - 1)) + 
     (m * n) / (total * (total - 1)) + 
     (m * (m - 1) * 0.25) / (total * (total - 1)));
  
  // Output the result with 5 decimal places
  writeln(Format('%.5f', [probability]));
end.
```

## Alternative Cleaner Approach

```pascal
program MendelsFirstLaw;
uses math;

var
  k, m, n: longint;
  total: real;
  p_recessive: real;

begin
  // Read input values
  readln(k, m, n);
  
  // Calculate total number of organisms
  total := k + m + n;
  
  // Calculate probability of recessive offspring
  // Case 1: Both parents are homozygous recessive (aa)
  p_recessive := (n / total) * ((n - 1) / (total - 1));
  
  // Case 2: One parent is heterozygous (Aa) and other is recessive (aa)
  p_recessive := p_recessive + 2 * (m / total) * (n / (total - 1)) * 0.5;
  
  // Case 3: Both parents are heterozygous (Aa)
  p_recessive := p_recessive + (m / total) * ((m - 1) / (total - 1)) * 0.25;
  
  // Probability of dominant phenotype = 1 - probability of recessive
  writeln(Format('%.5f', [1.0 - p_recessive]));
end.
```

## Explanation

The solution calculates the probability that two randomly selected organisms will produce offspring with a dominant allele. 

Key steps:
1. Calculate total number of organisms
2. Compute the probability of getting recessive offspring through all possible parent combinations:
   - Two homozygous recessive parents (aa × aa) → 100% recessive offspring
   - One heterozygous and one recessive parent (Aa × aa) → 50% recessive offspring  
   - Two heterozygous parents (Aa × Aa) → 25% recessive offspring
3. Subtract the recessive probability from 1 to get the dominant probability

The output format requires 5 decimal places as specified in the Rosalind problem.

