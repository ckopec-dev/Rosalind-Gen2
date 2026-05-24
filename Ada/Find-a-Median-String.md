# Rosalind Problem: Find_a_Median_String in Ada

## Problem Understanding

The Find_a_Median_String problem asks us to find a k-mer (substring of length k) that minimizes the sum of Hamming distances to all strings in a given collection of DNA strings.

## Solution Approach

1. Generate all possible k-mers of length k from the DNA alphabet {A, C, G, T}
2. For each k-mer, calculate the minimum Hamming distance to any string in the input
3. Find the k-mer with the minimum total distance across all input strings

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Find_a_Median_String is
   
   type DNA_String is array (Positive range <>) of Character;
   package DNA_Vector is new Ada.Containers.Vectors (Positive, DNA_String);
   type DNA_List is array (Positive range <>) of DNA_String;
   
   -- Function to calculate Hamming distance between two DNA strings
   function Hamming_Distance (S1, S2 : DNA_String) return Natural is
      Distance : Natural := 0;
   begin
      for I in S1'First .. S1'Last loop
         if S1(I) /= S2(I) then
            Distance := Distance + 1;
         end if;
      end loop;
      return Distance;
   end Hamming_Distance;
   
   -- Function to calculate minimum Hamming distance from a pattern to a DNA string
   function Min_Hamming_Distance (Pattern : DNA_String; Text : DNA_String) return Natural is
      Min_Dist : Natural := Integer'Last;
      K : constant Natural := Pattern'Length;
   begin
      for I in Text'First .. Text'Last - K + 1 loop
         declare
            Substring : DNA_String (1 .. K);
         begin
            for J in 1 .. K loop
               Substring(J) := Text(I + J - 1);
            end loop;
            declare
               Dist : constant Natural := Hamming_Distance (Pattern, Substring);
            begin
               if Dist < Min_Dist then
                  Min_Dist := Dist;
               end if;
            end;
         end;
      end loop;
      return Min_Dist;
   end Min_Hamming_Distance;
   
   -- Function to calculate total distance from a pattern to all DNA strings
   function Total_Distance (Pattern : DNA_String; DNA_List : DNA_List) return Natural is
      Total : Natural := 0;
   begin
      for I in DNA_List'First .. DNA_List'Last loop
         Total := Total + Min_Hamming_Distance (Pattern, DNA_List(I));
      end loop;
      return Total;
   end Total_Distance;
   
   -- Function to generate all k-mers of given length
   procedure Generate_Kmers (K : Natural; Kmers : in out DNA_Vector.Vector) is
      Alphabet : constant array (1..4) of Character := ('A', 'C', 'G', 'T');
      procedure Generate (Current : DNA_String; Position : Positive) is
      begin
         if Position > Current'Length then
            DNA_Vector.Append (Kmers, Current);
            return;
         end if;
         
         for C in Alphabet'Range loop
            Current(Position) := Alphabet(C);
            Generate (Current, Position + 1);
         end loop;
      end Generate;
   begin
      if K = 0 then
         return;
      end if;
      
      declare
         Current : DNA_String (1..K);
      begin
         Generate (Current, 1);
      end;
   end Generate_Kmers;
   
   -- Main function to find median string
   function Find_Median_String (DNA_List : DNA_List; K : Natural) return DNA_String is
      Kmers : DNA_Vector.Vector;
      Best_Pattern : DNA_String;
      Best_Distance : Natural := Integer'Last;
   begin
      Generate_Kmers (K, Kmers);
      
      for I in DNA_Vector.First_Index (Kmers) .. DNA_Vector.Last_Index (Kmers) loop
         declare
            Current_Pattern : constant DNA_String := DNA_Vector.Element (Kmers, I);
            Current_Distance : constant Natural := Total_Distance (Current_Pattern, DNA_List);
         begin
            if Current_Distance < Best_Distance then
               Best_Distance := Current_Distance;
               Best_Pattern := Current_Pattern;
            end if;
         end;
      end loop;
      
      return Best_Pattern;
   end Find_Median_String;
   
   -- Example usage
   procedure Example is
      DNA_Strings : DNA_List (1..4) := (
         (1 => 'C', 2 => 'G', 3 => 'T', 4 => 'G', 5 => 'A', 6 => 'C', 7 => 'T', 8 => 'G'),
         (1 => 'C', 2 => 'A', 3 => 'A', 4 => 'G', 5 => 'A', 6 => 'C', 7 => 'T', 8 => 'G'),
         (1 => 'T', 2 => 'G', 3 => 'A', 4 => 'A', 5 => 'G', 6 => 'C', 7 => 'T', 8 => 'A'),
         (1 => 'T', 2 => 'G', 3 => 'G', 4 => 'C', 5 => 'G', 6 => 'A', 7 => 'C', 8 => 'T')
      );
      K : constant Natural := 3;
      Result : DNA_String;
   begin
      Result := Find_Median_String (DNA_Strings, K);
      Put_Line ("Median string: " & Result);
   end Example;
   
begin
   Example;
end Find_a_Median_String;
```

## Key Features of the Implementation

1. **Hamming Distance Calculation**: Computes the number of differences between two DNA strings of equal length
2. **Minimum Distance Finding**: For a pattern, finds the minimum Hamming distance to any substring of a DNA string
3. **Total Distance Calculation**: Sums up minimum distances across all DNA strings for a given pattern
4. **K-mer Generation**: Generates all possible k-mers of a given length using recursive backtracking
5. **Median String Search**: Finds the k-mer that minimizes the total distance to all input strings

## Time Complexity

- Generating all k-mers: O(4^k)
- For each k-mer, calculating total distance: O(n × m × k) where n is number of strings, m is average string length
- Overall: O(4^k × n × m × k)

## Space Complexity

O(4^k × k) for storing all k-mers

This implementation handles the core requirements of the Rosalind Find_a_Median_String problem in Ada, providing a complete solution for finding the median string that minimizes the sum of distances to all input DNA strings.

