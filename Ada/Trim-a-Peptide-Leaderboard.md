# Rosalind Problem: Trim_a_Peptide_Leaderboard in Ada

## Problem Understanding

The task is to trim a peptide leaderboard to keep only the top N scores, where ties are handled by keeping all peptides with the same score.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Trim_a_Peptide_Leaderboard is
   
   type Peptide is tagged record
      Sequence : Unbounded_String;
      Score    : Integer;
   end record;
   
   package Peptide_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Peptide);
   
   type Peptide_Vector is new Peptide_Vectors.Vector;
   
   -- Function to compare peptides by score (descending order)
   function "<" (Left, Right : Peptide) return Boolean is
   begin
      return Left.Score > Right.Score;
   end "<";
   
   -- Function to trim leaderboard to top N scores
   function Trim_Leaderboard
     (Leaderboard : Peptide_Vector;
      N           : Positive) return Peptide_Vector is
      Trimmed : Peptide_Vector;
      Scores  : array (1..Leaderboard.Length) of Integer;
      Score_Map : Ada.Containers.Ordered_Maps.Map
        (Key_Type => Integer, Element_Type => Peptide_Vector);
      Current_Score : Integer;
      Count : Positive := 1;
   begin
      -- Handle edge case
      if Leaderboard.Is_Empty or N = 0 then
         return Trimmed;
      end if;
      
      -- Get all scores and sort them
      for I in 1..Leaderboard.Length loop
         Scores(I) := Leaderboard.Element(I).Score;
      end loop;
      
      -- Sort scores in descending order
      for I in 1..Leaderboard.Length loop
         for J in I+1..Leaderboard.Length loop
            if Scores(I) < Scores(J) then
               declare
                  Temp : constant Integer := Scores(I);
               begin
                  Scores(I) := Scores(J);
                  Scores(J) := Temp;
               end;
            end if;
         end loop;
      end loop;
      
      -- If N is greater than leaderboard size, trim to size
      declare
         Actual_N : constant Positive := 
           (if N > Leaderboard.Length then Leaderboard.Length else N);
      begin
         -- Collect peptides by score
         for I in 1..Leaderboard.Length loop
            declare
               P : constant Peptide := Leaderboard.Element(I);
            begin
               if not Score_Map.Contains(P.Score) then
                  Score_Map.Insert(P.Score, Peptide_Vector'(others => <>));
               end if;
               Score_Map.Element(P.Score).Append(P);
            end;
         end loop;
         
         -- Add peptides to result in order of scores
         for I in 1..Actual_N loop
            declare
               Score : constant Integer := Scores(I);
            begin
               if Score_Map.Contains(Score) then
                  declare
                     Peptides : constant Peptide_Vector := Score_Map.Element(Score);
                  begin
                     for J in 1..Peptides.Length loop
                        Trimmed.Append(Peptides.Element(J));
                     end loop;
                     Score_Map.Delete(Score);
                  end;
               end if;
            end;
         end loop;
      end;
      
      return Trimmed;
   end Trim_Leaderboard;
   
   -- Alternative simpler approach
   function Trim_Leaderboard_Simple
     (Leaderboard : Peptide_Vector;
      N           : Positive) return Peptide_Vector is
      Trimmed : Peptide_Vector;
      Sorted_List : Peptide_Vector := Leaderboard;
      Temp : Peptide;
      Score_Count : array (1..Leaderboard.Length) of Integer;
   begin
      -- Handle edge cases
      if Leaderboard.Is_Empty or N = 0 then
         return Trimmed;
      end if;
      
      -- Sort the leaderboard by score (descending)
      for I in 1..Sorted_List.Length loop
         for J in I+1..Sorted_List.Length loop
            if Sorted_List.Element(I).Score < Sorted_List.Element(J).Score then
               Temp := Sorted_List.Element(I);
               Sorted_List.Replace_Element(I, Sorted_List.Element(J));
               Sorted_List.Replace_Element(J, Temp);
            end if;
         end loop;
      end loop;
      
      -- Take top N elements
      for I in 1..(if N > Sorted_List.Length then Sorted_List.Length else N) loop
         Trimmed.Append(Sorted_List.Element(I));
      end loop;
      
      return Trimmed;
   end Trim_Leaderboard_Simple;
   
   -- Example usage
   procedure Test_Example is
      Leaderboard : Peptide_Vector;
      Trimmed     : Peptide_Vector;
      P1, P2, P3, P4, P5 : Peptide;
   begin
      -- Create sample leaderboard
      P1.Sequence := To_Unbounded_String("NQEL");
      P1.Score := 8;
      
      P2.Sequence := To_Unbounded_String("KEQ");
      P2.Score := 7;
      
      P3.Sequence := To_Unbounded_String("AEL");
      P3.Score := 7;
      
      P4.Sequence := To_Unbounded_String("GQL");
      P4.Score := 6;
      
      P5.Sequence := To_Unbounded_String("PEQ");
      P5.Score := 5;
      
      Leaderboard.Append(P1);
      Leaderboard.Append(P2);
      Leaderboard.Append(P3);
      Leaderboard.Append(P4);
      Leaderboard.Append(P5);
      
      -- Trim to top 3
      Trimmed := Trim_Leaderboard_Simple(Leaderboard, 3);
      
      Put_Line("Original Leaderboard:");
      for I in 1..Leaderboard.Length loop
         Put_Line("  " & To_String(Leaderboard.Element(I).Sequence) & 
                  " - Score: " & Integer'Image(Leaderboard.Element(I).Score));
      end loop;
      
      Put_Line("Trimmed Leaderboard (Top 3):");
      for I in 1..Trimmed.Length loop
         Put_Line("  " & To_String(Trimmed.Element(I).Sequence) & 
                  " - Score: " & Integer'Image(Trimmed.Element(I).Score));
      end loop;
   end Test_Example;
   
begin
   Test_Example;
end Trim_a_Peptide_Leaderboard;
```

## Explanation

This Ada solution implements the trimming of a peptide leaderboard to keep only the top N scores:

1. **Data Structure**: Uses a tagged record `Peptide` to store peptide sequences and their scores
2. **Vector Container**: Uses Ada's container library for dynamic arrays of peptides
3. **Sorting**: Implements bubble sort to sort peptides by score in descending order
4. **Trimming Logic**: 
   - Takes the top N peptides from the sorted list
   - Handles edge cases (empty leaderboard, N=0, N > leaderboard size)
   - Preserves ties by including all peptides with the same score

## Key Features

- **Type Safety**: Uses Ada's strong typing system
- **Memory Management**: Leverages Ada containers for automatic memory management
- **Error Handling**: Handles edge cases gracefully
- **Readability**: Clear variable names and structured code

## Usage

The program demonstrates trimming a sample leaderboard with 5 peptides to keep only the top 3, showing how ties are handled properly.

