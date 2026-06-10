# Rosalind Problem: Finding a Motif in DNA (Ada Solution)

## Problem Statement
Given two DNA strings s and t, find all locations of t as a substring of s.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Finding_A_Motif_in_DNA is
   -- Function to find all occurrences of a motif in DNA sequence
   function Find_Motif (DNA : Unbounded_String; Motif : Unbounded_String) 
      return String is
      Result : String(1..1000);  -- Assuming maximum 1000 occurrences
      Count  : Natural := 0;
      Pos    : Natural;
   begin
      Pos := 1;
      while Pos <= Length(DNA) - Length(Motif) + 1 loop
         if Slice(DNA, Pos, Pos + Length(Motif) - 1) = To_String(Motif) then
            Count := Count + 1;
            Result(Count) := Character'Pos(Character'Val(Pos));
         end if;
         Pos := Pos + 1;
      end loop;
      
      return Result(1..Count);
   end Find_Motif;
   
   -- Read input from stdin
   DNA_Seq : Unbounded_String;
   Motif   : Unbounded_String;
   Position : Natural;
begin
   -- Read DNA sequence and motif
   Get_Line(DNA_Seq);
   Get_Line(Motif);
   
   -- Find all occurrences of motif in DNA sequence
   Position := 1;
   while Position <= Length(DNA_Seq) - Length(Motif) + 1 loop
      if Slice(DNA_Seq, Position, Position + Length(Motif) - 1) = To_String(Motif) then
         Put(Position, 10);  -- Print position (1-indexed)
         Put(" ");
      end if;
      Position := Position + 1;
   end loop;
   
   New_Line;
end Finding_A_Motif_in_DNA;
```

## Corrected and Improved Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Finding_A_Motif_in_DNA is
   -- Read DNA sequence and motif from input
   DNA : Unbounded_String;
   Motif : Unbounded_String;
   Position : Natural;
begin
   -- Read the DNA sequence and motif
   Get_Line(DNA);
   Get_Line(Motif);
   
   -- Find all occurrences of motif in DNA sequence
   Position := 1;
   while Position <= Length(DNA) - Length(Motif) + 1 loop
      -- Check if motif matches at current position
      if Slice(DNA, Position, Position + Length(Motif) - 1) = To_String(Motif) then
         Put(Position);  -- Print position (1-indexed)
         Put(" ");
      end if;
      Position := Position + 1;
   end loop;
   
   New_Line;
end Finding_A_Motif_in_DNA;
```

## Alternative Solution with Proper Output Formatting

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Finding_A_Motif_in_DNA is
   DNA : Unbounded_String;
   Motif : Unbounded_String;
   Position : Natural;
   First : Boolean := True;
begin
   -- Read the DNA sequence and motif
   Get_Line(DNA);
   Get_Line(Motif);
   
   -- Find all occurrences of motif in DNA sequence
   Position := 1;
   while Position <= Length(DNA) - Length(Motif) + 1 loop
      -- Check if motif matches at current position
      if Slice(DNA, Position, Position + Length(Motif) - 1) = To_String(Motif) then
         if not First then
            Put(" ");
         end if;
         Put(Position);  -- Print position (1-indexed)
         First := False;
      end if;
      Position := Position + 1;
   end loop;
   
   New_Line;
end Finding_A_Motif_in_DNA;
```

## Key Points:
- The solution reads two strings from standard input: DNA sequence and motif
- It searches for all occurrences of the motif in the DNA sequence
- Positions are 1-indexed as required by Rosalind problems
- Output is space-separated positions without trailing spaces
- Uses Ada's Unbounded_String for flexible string handling
- The algorithm checks each possible position where motif could fit and compares substrings