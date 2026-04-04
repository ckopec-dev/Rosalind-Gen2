# Rosalind Problem: Mendel's First Law (Problem ID: IPRB)

## Problem Statement
Given: Three positive integers k, m, and n representing the number of organisms of each genotype:
- k: homozygous dominant individuals (AA)
- m: heterozygous individuals (Aa)
- n: homozygous recessive individuals (aa)

Return: The probability that two randomly selected mating organisms will produce an individual possessing a dominant allele (i.e., the probability of getting an A_ genotype).

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Mendels_First_Law is
   k, m, n : Integer;
   total   : Float;
   prob    : Float;
begin
   -- Read input values
   Put("Enter k (homozygous dominant): ");
   Get(k);
   Put("Enter m (heterozygous): ");
   Get(m);
   Put("Enter n (homozygous recessive): ");
   Get(n);
   
   -- Calculate total number of organisms
   total := Float(k + m + n);
   
   -- Calculate probability of getting dominant allele
   -- This is 1 minus the probability of getting two recessive alleles
   
   -- Probability of selecting two recessive individuals (aa)
   -- P(aa) = (n/total) * ((n-1)/(total-1))
   declare
      p_aa : Float := (Float(n) / total) * (Float(n - 1) / (total - 1.0));
   begin
      -- Probability of selecting one recessive and one heterozygous
      -- P(aa and Aa) = (n/total) * (m/(total-1)) + (m/total) * (n/(total-1))
      declare
         p_aaa : Float := (Float(n) / total) * (Float(m) / (total - 1.0)) +
                          (Float(m) / total) * (Float(n) / (total - 1.0));
      begin
         -- Probability of getting recessive offspring
         -- P(recessive) = P(aa) + P(aa and Aa) * 0.5
         -- (because Aa x aa produces 50% aa offspring)
         declare
            p_recessive : Float := p_aa + p_aaa * 0.5;
         begin
            -- Probability of dominant offspring = 1 - probability of recessive
            prob := 1.0 - p_recessive;
            Put("Probability of dominant allele: ");
            Put(prob, Fore => 1, Aft => 6, Exp => 0);
            New_Line;
         end;
      end;
   end;
   
end Mendels_First_Law;
```

## Alternative Cleaner Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Mendels_First_Law is
   k, m, n : Integer;
   total   : Float;
   prob    : Float;
begin
   -- Read input values
   Get(k);
   Get(m);
   Get(n);
   
   total := Float(k + m + n);
   
   -- Calculate probability of getting recessive offspring
   -- P(recessive) = P(both aa) + P(one aa, one Aa) * 0.5 + P(one Aa, one aa) * 0.5
   -- P(both aa) = (n/total) * ((n-1)/(total-1))
   -- P(one aa, one Aa) = 2 * (n/total) * (m/(total-1))
   -- P(one Aa, one aa) = 2 * (m/total) * (n/(total-1))
   
   -- Actually, let's simplify: P(recessive) = P(both aa) + P(one aa, one Aa) * 0.5
   -- But we also need to consider P(both Aa) producing recessive offspring
   -- P(both Aa) = (m/total) * ((m-1)/(total-1))
   -- P(both Aa) * 0.25 = probability of recessive offspring from two Aa parents
   
   -- Simpler approach: P(dominant) = 1 - P(recessive)
   
   -- P(recessive) = P(both aa) + P(both Aa) * 0.25 + P(one aa, one Aa) * 0.5
   declare
      p_aa : Float := (Float(n) / total) * (Float(n - 1) / (total - 1.0));
      p_aaa : Float := (Float(m) / total) * (Float(m - 1) / (total - 1.0)) * 0.25;
      p_aaa2 : Float := (Float(n) / total) * (Float(m) / (total - 1.0)) * 0.5;
      p_aaa3 : Float := (Float(m) / total) * (Float(n) / (total - 1.0)) * 0.5;
   begin
      prob := 1.0 - (p_aa + p_aaa + p_aaa2 + p_aaa3);
      Put("Probability of dominant allele: ");
      Put(prob, Fore => 1, Aft => 6, Exp => 0);
      New_Line;
   end;
   
end Mendels_First_Law;
```

## Mathematical Approach

The key insight is that we want the probability of getting a dominant allele (A_) in the offspring:

1. **Probability of getting two recessive alleles (aa):**
   - Both parents are aa: (n/total) × ((n-1)/(total-1))
   - One parent is aa, other is Aa: 2 × (n/total) × (m/(total-1)) × 0.5
   - Both parents are Aa: (m/total) × ((m-1)/(total-1)) × 0.25

2. **Final probability of dominant allele:**
   - P(dominant) = 1 - P(recessive)

The solution calculates this probability using the complement rule, which is more straightforward than calculating all the direct dominant cases.

