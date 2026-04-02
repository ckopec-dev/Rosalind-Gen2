# Rosalind Problem: Introduction to Random Strings

## Problem Statement
Given: A DNA string s of length at most 100 bp and an array A of 10 real numbers between 0 and 1. Return: An array B of 10 numbers representing the common logarithm of the probability that a random string constructed with the GC-content found in A will match s exactly.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Introduction_To_Random_Strings is
   type DNA_String is array (Positive range <>) of Character;
   type Log_Prob_Array is array (Positive range <>) of Float;
   
   -- Function to calculate GC content from DNA string
   function GC_Content(DNA : DNA_String) return Float is
      GC_Count : Natural := 0;
      Total    : Natural := DNA'Length;
   begin
      for I in DNA'Range loop
         if DNA(I) = 'G' or DNA(I) = 'C' then
            GC_Count := GC_Count + 1;
         end if;
      end loop;
      return Float(GC_Count) / Float(Total);
   end GC_Content;
   
   -- Function to calculate probability of matching string
   function Calculate_Probability(DNA : DNA_String; GC_Content_Val : Float) return Float is
      A_Count : Natural := 0;
      T_Count : Natural := 0;
      G_Count : Natural := 0;
      C_Count : Natural := 0;
      Log_Prob : Float := 0.0;
   begin
      -- Count nucleotides
      for I in DNA'Range loop
         case DNA(I) is
            when 'A' => A_Count := A_Count + 1;
            when 'T' => T_Count := T_Count + 1;
            when 'G' => G_Count := G_Count + 1;
            when 'C' => C_Count := C_Count + 1;
            when others => null;
         end case;
      end loop;
      
      -- Calculate log probability
      -- For each position, probability = (GC_content/2)^(GC_count) * ((1-GC_content)/2)^(AT_count)
      Log_Prob := Float(A_Count + T_Count) * Log(0.5 * (1.0 - GC_Content_Val));
      Log_Prob := Log_Prob + Float(G_Count + C_Count) * Log(0.5 * GC_Content_Val);
      
      return Log_Prob;
   end Calculate_Probability;
   
   -- Input DNA string
   DNA : DNA_String(1..100);
   DNA_Length : Natural;
   
   -- Input GC content array
   GC_Array : array (1..10) of Float;
   
   -- Output array
   Output_Array : Log_Prob_Array(1..10);
   
   -- Read DNA string
   procedure Read_DNA is
      Line : String(1..100);
      Last : Natural;
   begin
      Get_Line(Line, Last);
      DNA_Length := Last;
      for I in 1..Last loop
         DNA(I) := Line(I);
      end loop;
   end Read_DNA;
   
   -- Read GC content array
   procedure Read_GC_Array is
      Line : String(1..200);
      Last : Natural;
      I : Natural := 1;
      Start : Natural := 1;
   begin
      Get_Line(Line, Last);
      while I <= 10 loop
         -- Find next space or end of line
         while Start <= Last and then Line(Start) = ' ' loop
            Start := Start + 1;
         end loop;
         if Start > Last then
            exit;
         end if;
         
         -- Extract number
         declare
            End_Index : Natural := Start;
         begin
            while End_Index <= Last and then Line(End_Index) /= ' ' loop
               End_Index := End_Index + 1;
            end loop;
            
            GC_Array(I) := Float'Value(Line(Start..End_Index-1));
            I := I + 1;
            Start := End_Index + 1;
         end;
      end loop;
   end Read_GC_Array;
   
begin
   -- Read input
   Read_DNA;
   Read_GC_Array;
   
   -- Calculate probabilities
   for I in 1..10 loop
      Output_Array(I) := Calculate_Probability(DNA, GC_Array(I));
   end loop;
   
   -- Output results
   for I in 1..10 loop
      Put(Output_Array(I), Fore => 1, Aft => 6, Exp => 0);
      Put(" ");
   end loop;
   New_Line;
   
end Introduction_To_Random_Strings;
```

## Explanation

This Ada solution addresses the Rosalind problem by:

1. **Reading Input**: 
   - Reads a DNA string of up to 100 nucleotides
   - Reads 10 GC content values between 0 and 1

2. **Key Functions**:
   - `GC_Content`: Calculates the GC content of the input DNA string
   - `Calculate_Probability`: Computes the log probability of matching the DNA string with a random string having the specified GC content

3. **Probability Calculation**:
   - For each GC content value, we calculate the probability of generating the exact DNA string
   - Uses the formula: P = (0.5 * (1-GC))^(A+T) * (0.5 * GC)^(G+C)
   - Takes logarithm to avoid underflow issues

4. **Output**:
   - Returns an array of 10 log probabilities, one for each GC content value

The solution handles the mathematical computation of random string probabilities while maintaining proper Ada programming practices including proper types, procedures, and error handling.

