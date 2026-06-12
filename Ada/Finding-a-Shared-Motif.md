# Rosalind Problem: Finding a Shared Motif

## Problem Description
Given a collection of DNA strings, find the longest common substring that appears in all of them.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
procedure Finding_A_Shared_Motif is
   
   type DNA_String is new Unbounded_String;
   package DNA_Vector is new Ada.Containers.Vectors (Positive, DNA_String);
   use DNA_Vector;
   
   -- Function to get all substrings of a given string
   function Get_Substrings(S : DNA_String; Length : Natural) return Vector is
      Result : Vector;
      Substring : Unbounded_String;
   begin
      for I in 1..(Length(S) - Length + 1) loop
         Substring := To_Unbounded_String(Slice(S, I, I + Length - 1));
         Append(Result, Substring);
      end loop;
      return Result;
   end Get_Substrings;
   
   -- Function to check if a substring appears in all DNA strings
   function Is_Common_Substring(Sub : DNA_String; Sequences : Vector) return Boolean is
      Found : Boolean;
   begin
      for I in 1..Sequences.Length loop
         if Index(Sequences(Element(Sequences, I)), Sub) = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Common_Substring;
   
   -- Function to find the longest common substring
   function Longest_Common_Substring(Sequences : Vector) return DNA_String is
      Max_Length : Natural := 0;
      Best_Substring : DNA_String := Null_Unbounded_String;
      Current_Length : Natural := Length(Sequences(Element(Sequences, 1)));
   begin
      -- Check from longest possible substring down to shortest
      for L in reverse 1..Current_Length loop
         declare
            Substrings : Vector := Get_Substrings(Sequences(Element(Sequences, 1)), L);
         begin
            for I in 1..Substrings.Length loop
               if Is_Common_Substring(Substrings(Element(Substrings, I)), Sequences) then
                  return Substrings(Element(Substrings, I));
               end if;
            end loop;
         end;
      end loop;
      
      return Best_Substring;
   end Longest_Common_Substring;
   
   -- Read input from stdin
   function Read_DNA_Sequences return Vector is
      Result : Vector;
      Line : Unbounded_String;
   begin
      while not End_Of_File loop
         Get_Line(Line);
         if Length(Line) > 0 then
            Append(Result, Line);
         end if;
      end loop;
      return Result;
   end Read_DNA_Sequences;
   
begin
   -- Read all DNA sequences
   declare
      Sequences : Vector := Read_DNA_Sequences;
      Longest_Common : DNA_String;
   begin
      if Sequences.Length > 0 then
         Longest_Common := Longest_Common_Substring(Sequences);
         Put_Line(To_String(Longest_Common));
      end if;
   end;
   
end Finding_A_Shared_Motif;
```

## Approach Explanation

1. **Data Structures**: 
   - Use `Unbounded_String` to handle DNA sequences of varying lengths
   - Use `Vector` container to store multiple DNA strings

2. **Key Functions**:
   - `Get_Substrings`: Generate all substrings of a given length from a DNA string
   - `Is_Common_Substring`: Check if a substring appears in all DNA sequences
   - `Longest_Common_Substring`: Find the longest common substring by checking decreasing lengths

3. **Algorithm**:
   - Start with the longest possible substring (length of the first sequence)
   - Work down to shorter substrings
   - For each length, check all substrings from the first sequence
   - Return the first (longest) substring that appears in all sequences

## Time Complexity
O(n × m²) where n is the number of sequences and m is the average length of sequences.

## Space Complexity  
O(m²) for storing substrings during computation.

This solution handles multiple DNA sequences and finds the longest shared motif efficiently using a brute-force approach with early termination.