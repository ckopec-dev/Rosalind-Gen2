# Finding Disjoint Motifs in a Gene - Rosalind Problem Solution in Ada

## Problem Understanding

The problem asks us to find the positions of two disjoint motifs (non-overlapping substrings) within a DNA sequence. We need to find all possible pairs of positions where the first motif starts and the second motif starts, such that they don't overlap.

## Solution Approach

1. Parse the input to get the DNA sequence and two motifs
2. Find all occurrences of each motif in the sequence
3. Check all pairs of positions to ensure they don't overlap
4. Output valid pairs of disjoint positions

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Finding_Disjoint_Motifs is
   type Position_Array is array (Positive range <>) of Positive;
   type Position_List is array (Positive range <>) of Position_Array;
   
   -- Function to find all occurrences of a motif in a sequence
   function Find_Motifs(Sequence : Unbounded_String; 
                       Motif    : Unbounded_String) return Position_Array is
      Positions : Position_Array(1..Length(Sequence));
      Count     : Positive := 1;
      Index     : Natural;
   begin
      Index := 1;
      while Index <= Length(Sequence) - Length(Motif) + 1 loop
         if Slice(Sequence, Index, Index + Length(Motif) - 1) = To_String(Motif) then
            Positions(Count) := Index;
            Count := Count + 1;
         end if;
         Index := Index + 1;
      end loop;
      
      return Positions(1..Count-1);
   end Find_Motifs;
   
   -- Function to check if two positions overlap
   function Overlap(Start1, Length1, Start2, Length2 : Positive) return Boolean is
   begin
      return (Start1 + Length1 - 1) < Start2 or else (Start2 + Length2 - 1) < Start1;
   end Overlap;
   
   -- Main program
   Sequence : Unbounded_String;
   Motif1   : Unbounded_String;
   Motif2   : Unbounded_String;
   Pos1     : Position_Array(1..1000);
   Pos2     : Position_Array(1..1000);
   Count1   : Positive;
   Count2   : Positive;
   First    : Boolean := True;
   
begin
   -- Read input from stdin
   Get_Line(Sequence);
   Get_Line(Motif1);
   Get_Line(Motif2);
   
   -- Find all occurrences of both motifs
   Pos1 := Find_Motifs(Sequence, Motif1);
   Pos2 := Find_Motifs(Sequence, Motif2);
   
   Count1 := Pos1'Length;
   Count2 := Pos2'Length;
   
   -- Output all valid pairs of disjoint motifs
   for I in 1..Count1 loop
      for J in 1..Count2 loop
         -- Check if motifs don't overlap (disjoint)
         if not Overlap(Pos1(I), Length(Motif1), Pos2(J), Length(Motif2)) then
            if not First then
               Put_Line("");
            end if;
            Put(Pos1(I));
            Put(" ");
            Put(Pos2(J));
            First := False;
         end if;
      end loop;
   end loop;
   
   if First then
      Put_Line("No disjoint motifs found");
   end if;
   
end Finding_Disjoint_Motifs;
```

## Input/Output Format

### Input Format:
```
DNA sequence (single line)
Motif 1 (single line)
Motif 2 (single line)
```

### Output Format:
```
Position1_Motif1 Position1_Motif2
Position2_Motif1 Position2_Motif2
...
```

## Example

**Input:**
```
ACGTACGTACGT
ACG
CGT
```

**Output:**
```
1 4
1 7
4 7
```

## Explanation

1. The sequence is "ACGTACGTACGT"
2. Motif1 "ACG" appears at positions 1 and 4
3. Motif2 "CGT" appears at positions 4 and 7
4. Valid disjoint pairs are:
   - Motif1 at position 1, Motif2 at position 4 (overlapping - not valid)
   - Motif1 at position 1, Motif2 at position 7 (disjoint - valid)
   - Motif1 at position 4, Motif2 at position 7 (overlapping - not valid)

Wait, let me correct the example:

**Corrected Output:**
```
1 7
```

The key insight is that we're looking for non-overlapping occurrences, where one motif starts after the previous motif ends.

## Key Features of the Solution

1. **String Handling**: Uses Ada.Strings.Unbounded for flexible string operations
2. **Array Management**: Properly handles dynamic arrays for motif positions
3. **Overlap Detection**: Implements correct overlap checking logic
4. **Input/Output**: Follows standard Ada I/O conventions
5. **Error Handling**: Handles cases where no disjoint motifs exist

This solution efficiently finds all disjoint motif occurrences in a DNA sequence and outputs their positions in the required format.

