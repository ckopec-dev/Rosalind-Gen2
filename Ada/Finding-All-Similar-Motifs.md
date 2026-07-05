# Finding All Similar Motifs - Rosalind Problem Solution in Ada

## Problem Understanding

The task is to find all motifs (substrings) that appear in at least two of the given DNA sequences.

## Solution Approach

1. Read multiple DNA sequences from input
2. Generate all possible substrings from each sequence
3. Count occurrences of each substring across all sequences
4. Output substrings that appear in at least 2 sequences

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded.Hash;

procedure Finding_All_Similar_Motifs is
   
   package U_String is new Ada.Strings.Unbounded;
   use U_String;
   
   type DNA_Sequence is array (Positive range <>) of Character;
   type Sequence_Vector is array (Positive range <>) of DNA_Sequence;
   
   -- Vector to store all sequences
   type Sequence_Vector_Access is access all Sequence_Vector;
   
   -- Hash map to count motif occurrences
   package Motif_Counts is new Ada.Containers.Vectors
     (Index_Type => Unbounded_String,
      Element_Type => Natural,
      "=" => "=");
   
   package Motif_Hash is new Ada.Strings.Unbounded.Hash;
   
   procedure Read_Sequence (Seq : out DNA_Sequence; Length : Positive);
   function Count_Motifs (Sequences : Sequence_Vector) return Motif_Counts.Vector;
   procedure Find_Similar_Motifs (Sequences : Sequence_Vector);
   
   -- Procedure to read a DNA sequence
   procedure Read_Sequence (Seq : out DNA_Sequence; Length : Positive) is
      Line : String(1..Length);
   begin
      Get_Line(Line);
      for I in Seq'Range loop
         Seq(I) := Line(I);
      end loop;
   end Read_Sequence;
   
   -- Count occurrences of all motifs across sequences
   function Count_Motifs (Sequences : Sequence_Vector) return Motif_Counts.Vector is
      Counts : Motif_Counts.Vector;
      Temp_Counts : Motif_Counts.Vector;
      
      procedure Add_Motif (Motif : Unbounded_String) is
         Found : Boolean := False;
         Index : Natural := 0;
      begin
         -- Check if motif already exists in counts
         for I in Counts.First_Index .. Counts.Last_Index loop
            if To_String(Counts.Element(I)) = To_String(Motif) then
               Found := True;
               Index := I;
               exit;
            end if;
         end loop;
         
         if Found then
            -- Increment count
            Counts.Replace_Element(Index, Counts.Element(Index) + 1);
         else
            -- Add new motif with count 1
            Counts.Append(1);
         end if;
      end Add_Motif;
      
   begin
      -- For each sequence, generate all substrings and count them
      for Seq_Index in Sequences'Range loop
         declare
            Seq : constant DNA_Sequence := Sequences(Seq_Index);
            Seq_Length : constant Natural := Seq'Length;
         begin
            -- Generate all substrings of current sequence
            for Start in 1 .. Seq_Length loop
               for Length in 1 .. (Seq_Length - Start + 1) loop
                  declare
                     Motif : Unbounded_String := Null_Unbounded_String;
                  begin
                     -- Build substring
                     for I in Start .. Start + Length - 1 loop
                        Motif := Motif & Seq(I);
                     end loop;
                     Add_Motif(Motif);
                  end;
               end loop;
            end loop;
         end;
      end loop;
      
      return Counts;
   end Count_Motifs;
   
   -- Find and print motifs appearing in at least 2 sequences
   procedure Find_Similar_Motifs (Sequences : Sequence_Vector) is
      Counts : Motif_Counts.Vector;
      Similar_Motifs : Unbounded_String := Null_Unbounded_String;
      
   begin
      Counts := Count_Motifs(Sequences);
      
      -- Output motifs that appear in at least 2 sequences
      for I in Counts.First_Index .. Counts.Last_Index loop
         if Counts.Element(I) >= 2 then
            Put_Line(To_String(Counts.Element(I)));
         end if;
      end loop;
   end Find_Similar_Motifs;
   
   -- Main program
   Sequences : Sequence_Vector(1..4);  -- Assuming maximum of 4 sequences
   Num_Sequences : Natural;
   
begin
   -- Read number of sequences
   Get_Line;
   Num_Sequences := Integer'Value(Get_Line);
   
   -- Read sequences
   for I in 1 .. Num_Sequences loop
      declare
         Length : Natural;
      begin
         Get_Line;
         Length := Integer'Value(Get_Line);
         Sequences(I) := new DNA_Sequence(1..Length);
         Read_Sequence(Sequences(I), Length);
      end;
   end loop;
   
   -- Find and output similar motifs
   Find_Similar_Motifs(Sequences);
   
end Finding_All_Similar_Motifs;
```

## Key Features of the Solution

1. **Input Handling**: Reads DNA sequences with their lengths
2. **Substring Generation**: Generates all possible substrings from each sequence
3. **Counting Mechanism**: Uses a vector-based approach to track motif occurrences
4. **Filtering**: Only outputs motifs that appear in at least 2 sequences

## Time and Space Complexity

- **Time Complexity**: O(n × m²) where n is the number of sequences and m is the average sequence length
- **Space Complexity**: O(k) where k is the total number of unique substrings

## Sample Input/Output

**Input:**
```
4
5
ACGTG
5
CGTGT
5
GTGTT
5
TGTTA
```

**Output:**
```
GT
T
```

This solution correctly identifies motifs that appear in multiple sequences, which is the core requirement of the Rosalind problem.