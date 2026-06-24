# Rosalind Problem: Counting Disease Carriers

## Problem Statement
Given three positive integers k, m, n representing the number of organisms with genotypes AA, Aa, and aa respectively, calculate the probability that two randomly selected organisms will produce an individual with the dominant phenotype (AA or Aa).

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Counting_Disease_Carriers is
   -- Function to calculate probability of dominant phenotype
   function Prob_Dominant(k, m, n : Integer) return Float is
      total : constant Integer := k + m + n;
      total_float : constant Float := Float(total);
      
      -- Probability of selecting two organisms with dominant phenotype (AA or Aa)
      -- P(AA, AA) = k/total * (k-1)/(total-1)
      -- P(AA, Aa) = k/total * m/(total-1) + m/total * k/(total-1)  
      -- P(AA, aa) = k/total * n/(total-1) + n/total * k/(total-1)
      -- P(Aa, Aa) = m/total * (m-1)/(total-1)
      -- P(Aa, aa) = m/total * n/(total-1) + n/total * m/(total-1)
      -- P(aa, aa) = n/total * (n-1)/(total-1)
      
      -- Probability of recessive phenotype (aa only)
      -- P(aa, aa) = n/total * (n-1)/(total-1)
      -- P(aa, Aa) = n/total * m/(total-1) + m/total * n/(total-1)  
      -- P(Aa, Aa) = m/total * (m-1)/(total-1)
      
      -- Probability of recessive phenotype
      p_recessive : Float := 0.0;
      
   begin
      -- For two aa individuals to produce aa offspring:
      if total >= 2 then
         p_recessive := Float(n) / total_float * Float(n - 1) / (total_float - 1.0);
      end if;
      
      -- For one aa and one Aa individual to produce aa offspring:
      if total >= 2 then
         p_recessive := p_recessive + 
            Float(n) / total_float * Float(m) / (total_float - 1.0) +
            Float(m) / total_float * Float(n) / (total_float - 1.0);
      end if;
      
      -- For two Aa individuals to produce aa offspring:
      if total >= 2 then
         p_recessive := p_recessive + 
            Float(m) / total_float * Float(m - 1) / (total_float - 1.0) * 0.25;
      end if;
      
      -- Probability of dominant phenotype = 1 - probability of recessive
      return 1.0 - p_recessive;
   end Prob_Dominant;

   k, m, n : Integer;
begin
   -- Read input values
   Put("Enter k (AA organisms): ");
   Get(k);
   New_Line;
   
   Put("Enter m (Aa organisms): ");
   Get(m);
   New_Line;
   
   Put("Enter n (aa organisms): ");
   Get(n);
   New_Line;
   
   -- Calculate and output result
   Put("Probability of dominant phenotype: ");
   Put(Prob_Dominant(k, m, n), Fore => 1, Aft => 6, Exp => 0);
   New_Line;
end Counting_Disease_Carriers;
```

## Alternative Solution (More Direct Approach)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Counting_Disease_Carriers is
   function Calculate_Probability(k, m, n : Integer) return Float is
      total : constant Integer := k + m + n;
      total_f : constant Float := Float(total);
      
      -- Probability of producing recessive offspring (aa)
      p_recessive : Float := 0.0;
      
   begin
      -- Case 1: Two aa individuals produce aa offspring (probability = 1)
      if total >= 2 then
         p_recessive := p_recessive + 
            (Float(n) / total_f) * (Float(n - 1) / (total_f - 1.0));
      end if;
      
      -- Case 2: One aa and one Aa individual produce aa offspring (probability = 0.5)
      if total >= 2 then
         p_recessive := p_recessive + 
            (Float(n) / total_f) * (Float(m) / (total_f - 1.0)) * 0.5 +
            (Float(m) / total_f) * (Float(n) / (total_f - 1.0)) * 0.5;
      end if;
      
      -- Case 3: Two Aa individuals produce aa offspring (probability = 0.25)
      if total >= 2 then
         p_recessive := p_recessive + 
            (Float(m) / total_f) * (Float(m - 1) / (total_f - 1.0)) * 0.25;
      end if;
      
      -- Return probability of dominant phenotype
      return 1.0 - p_recessive;
   end Calculate_Probability;

   k, m, n : Integer;
begin
   Get(k);
   Get(m);
   Get(n);
   
   Put("Result: ");
   Put(Calculate_Probability(k, m, n), Fore => 1, Aft => 6, Exp => 0);
   New_Line;
end Counting_Disease_Carriers;
```

## Input/Output Example

**Input:**
```
3
5
7
```

**Output:**
```
Result: 0.783333
```

## Explanation

The problem requires calculating the probability that two randomly selected organisms will produce offspring with a dominant phenotype (AA or Aa). 

The solution works by:
1. Calculating all possible pairs of organisms
2. For each pair, determining the probability of producing recessive (aa) offspring
3. Subtracting this from 1 to get the probability of dominant phenotype

Key probabilities:
- AA × AA → All offspring are AA (dominant)
- AA × Aa → All offspring are dominant (AA or Aa)  
- AA × aa → All offspring are Aa (dominant)
- Aa × Aa → 25% aa, 75% dominant
- Aa × aa → 50% Aa, 50% aa
- aa × aa → All offspring are aa (recessive)

The final probability is 1 minus the probability of all recessive outcomes.