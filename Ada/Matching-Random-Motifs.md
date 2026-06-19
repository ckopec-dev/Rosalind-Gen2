# Rosalind Problem: Matching Random Motifs (Ada Solution)

## Problem Understanding

In this problem, we need to calculate the probability that a random DNA string of given length will contain a specific motif (pattern) with high probability.

## Solution Approach

We'll use the complement probability approach:
1. Calculate the probability that a single position matches the motif
2. Use the complement to find the probability that no match occurs
3. Apply the binomial distribution formula

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Matching_Random_Motifs is
   
   -- Function to calculate probability of matching motif at position
   function Prob_Match(Motif : String; Sequence : String) return Float is
      Match_Count : Integer := 0;
      Total_Length : Integer := Motif'Length;
   begin
      for I in 1..Motif'Length loop
         if Motif(I) = Sequence(I) then
            Match_Count := Match_Count + 1;
         end if;
      end loop;
      
      return Float(Match_Count) / Float(Total_Length);
   end Prob_Match;
   
   -- Function to calculate probability of not matching motif at all positions
   function Prob_No_Match(Motif : String; Sequence : String; N : Integer) return Float is
      P_Match : Float := Prob_Match(Motif, Sequence);
      P_No_Match : Float := 1.0 - P_Match;
   begin
      -- Probability that no position matches the motif
      return Power(P_No_Match, Float(N));
   end Prob_No_Match;
   
   -- Function to calculate probability of at least one match
   function Prob_At_Least_One_Match(Motif : String; Sequence : String; N : Integer) return Float is
      P_No_Match : Float := Prob_No_Match(Motif, Sequence, N);
   begin
      return 1.0 - P_No_Match;
   end Prob_At_Least_One_Match;
   
   -- Main calculation function
   procedure Calculate_Probability(Motif : String; Length : Integer) is
      DNA : String(1..Length) := (others => 'A');
      P : Float;
   begin
      -- For this specific problem, we need to calculate the probability
      -- that a random string of given length contains the motif
      Put_Line("Motif: " & Motif);
      Put_Line("Sequence Length: " & Integer'Image(Length));
      
      -- Simple approach: for each position in the motif,
      -- we calculate the probability of match with random DNA
      P := 1.0;
      for I in 1..Motif'Length loop
         case Motif(I) is
            when 'A' | 'T' => P := P * 0.25;  -- A or T = 0.25 each
            when 'G' | 'C' => P := P * 0.25;  -- G or C = 0.25 each
            when others => null;
         end case;
      end loop;
      
      Put("Probability of motif occurrence: ");
      Put(P, Fore => 1, Aft => 6, Exp => 0);
      New_Line;
   end Calculate_Probability;
   
   -- More accurate approach using binomial distribution
   procedure Calculate_Binomial(Motif : String; Length : Integer) is
      Match_P : Float := 1.0;
      P : Float;
      N : Integer := Length - Motif'Length + 1;
   begin
      -- Calculate probability of matching at one position
      for I in 1..Motif'Length loop
         case Motif(I) is
            when 'A' | 'T' => Match_P := Match_P * 0.25;
            when 'G' | 'C' => Match_P := Match_P * 0.25;
            when others => null;
         end case;
      end loop;
      
      -- Probability of NOT matching at one position
      P := 1.0 - Match_P;
      
      -- Probability that NO match occurs in all positions
      P := Power(P, Float(N));
      
      -- Probability of AT LEAST ONE match
      P := 1.0 - P;
      
      Put("Probability of motif occurrence (Binomial): ");
      Put(P, Fore => 1, Aft => 6, Exp => 0);
      New_Line;
   end Calculate_Binomial;
   
begin
   -- Example usage
   Put_Line("Rosalind: Matching Random Motifs");
   Put_Line("===============================");
   
   -- Sample motif and length - these would come from input in real problem
   declare
      Sample_Motif : constant String := "ATGC";
      Sample_Length : constant Integer := 10;
   begin
      Calculate_Probability(Sample_Motif, Sample_Length);
      Calculate_Binomial(Sample_Motif, Sample_Length);
   end;
   
end Matching_Random_Motifs;
```

## Key Points

1. **Probability Calculation**: For each position in the motif, we calculate the probability of matching (0.25 for each nucleotide)
2. **Binomial Approach**: We consider that there are N possible positions where the motif can start
3. **Complement Method**: We calculate the probability of no match occurring, then subtract from 1 to get at least one match

## Input/Output Format

For Rosalind problems, typical input would be:
- First line: Length of DNA sequence (N)
- Second line: Motif pattern (string)

The output should be the probability that a random DNA string of length N contains the motif.

This Ada solution provides both the basic calculation approach and the more accurate binomial distribution method for solving the matching random motifs problem.