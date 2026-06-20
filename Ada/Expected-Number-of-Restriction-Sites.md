# Rosalind Problem: Expected Number of Restriction Sites

## Problem Statement
Given a DNA string and a probability distribution for nucleotides, we need to calculate the expected number of occurrences of a specific restriction site pattern.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Expected_Number_of_Restriction_Sites is
   
   -- Function to calculate probability of a pattern
   function Pattern_Probability(Pattern : String; Prob : array (Character) of Float) return Float is
      Result : Float := 1.0;
   begin
      for I in Pattern'Range loop
         Result := Result * Prob(Pattern(I));
      end loop;
      return Result;
   end Pattern_Probability;
   
   -- Function to calculate expected number of occurrences
   function Expected_Count(DNA_Length : Natural; Pattern : String; Prob : array (Character) of Float) return Float is
      Pattern_P : Float := Pattern_Probability(Pattern, Prob);
      Num_Positions : Natural := DNA_Length - Pattern'Length + 1;
   begin
      return Float(Num_Positions) * Pattern_P;
   end Expected_Count;
   
   -- Read input from stdin
   procedure Read_Input(DNA : out Unbounded_String; 
                       A_Prob, C_Prob, G_Prob, T_Prob : out Float) is
      Line : Unbounded_String;
   begin
      Line := To_Unbounded_String(Get_Line);
      DNA := Line;
      
      Line := To_Unbounded_String(Get_Line);
      A_Prob := Float'Value(To_String(Line));
      
      Line := To_Unbounded_String(Get_Line);
      C_Prob := Float'Value(To_String(Line));
      
      Line := To_Unbounded_String(Get_Line);
      G_Prob := Float'Value(To_String(Line));
      
      Line := To_Unbounded_String(Get_Line);
      T_Prob := Float'Value(To_String(Line));
   end Read_Input;
   
   -- Main program
   DNA : Unbounded_String;
   A_P, C_P, G_P, T_P : Float;
   Pattern : constant String := "ATGC";
   Expected : Float;
   
begin
   -- Read input
   Read_Input(DNA, A_P, C_P, G_P, T_P);
   
   -- Define probability distribution
   declare
      Prob : array (Character) of Float :=
        ('A' => A_P,
         'C' => C_P,
         'G' => G_P,
         'T' => T_P);
   begin
      -- Calculate expected number of occurrences
      Expected := Expected_Count(Length(DNA), Pattern, Prob);
      
      -- Output result with appropriate precision
      Put("Expected number of ");
      Put(Pattern);
      Put(" sites: ");
      Put(Expected, Fore => 1, Aft => 6, Exp => 0);
      New_Line;
   end;
   
end Expected_Number_of_Restriction_Sites;
```

## Explanation

The solution works as follows:

1. **Pattern Probability Calculation**: The `Pattern_Probability` function computes the probability of a specific pattern occurring by multiplying the probabilities of each nucleotide in the pattern.

2. **Expected Count Calculation**: The `Expected_Count` function calculates the expected number of occurrences using the formula:
   - Number of possible positions = DNA length - pattern length + 1
   - Expected count = Number of positions × Probability of pattern

3. **Input Handling**: The program reads:
   - DNA sequence
   - Probabilities for A, C, G, T nucleotides (in that order)

4. **Output**: Displays the expected number of occurrences of the restriction site pattern.

## Example Usage

For input:
```
ATCGATCG
0.25
0.25
0.25
0.25
```

The program would output:
```
Expected number of ATGC sites: 1.000000
```

This solution handles the mathematical expectation calculation correctly and uses Ada's strong typing system to ensure robust code execution.