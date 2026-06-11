# Rosalind Problem: Overlap Graphs (Ada Solution)

## Problem Description
Given a collection of DNA strings, construct a directed graph where each string is a node, and there's a directed edge from string A to string B if the suffix of length k of A matches the prefix of length k of B.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Overlap_Graphs is
   type DNA_String is tagged record
      ID   : Unbounded_String;
      Seq  : Unbounded_String;
   end record;

   package DNA_Vectors is new Ada.Containers.Vectors (Positive, DNA_String);
   use DNA_Vectors;

   function Prefix(S : Unbounded_String; Length : Positive) return Unbounded_String is
   begin
      if Length >= Length(S) then
         return S;
      else
         return Head(S, Length);
      end if;
   end Prefix;

   function Suffix(S : Unbounded_String; Length : Positive) return Unbounded_String is
   begin
      if Length >= Length(S) then
         return S;
      else
         return Tail(S, Length);
      end if;
   end Suffix;

   procedure Read_Fasta(Sequences : in out Vector) is
      Line : Unbounded_String;
      Current : DNA_String;
      First_Line : Boolean := True;
   begin
      loop
         exit when not (Ada.Text_IO.Get_Line(Line));
         if Length(Line) > 0 then
            if Element(Line, 1) = '>' then
               if not First_Line then
                  Append(Sequences, Current);
               end if;
               Current.ID := Tail(Line, Length(Line) - 1);
               Current.Seq := Null_Unbounded_String;
               First_Line := False;
            else
               Append(Current.Seq, Line);
            end if;
         end if;
      end loop;
      
      if not First_Line then
         Append(Sequences, Current);
      end if;
   end Read_Fasta;

   procedure Print_Overlap_Graph(Sequences : Vector; K : Positive) is
      Found_Edge : Boolean;
   begin
      Found_Edge := False;
      for I in 1 .. Sequences.Length loop
         for J in 1 .. Sequences.Length loop
            if I /= J then
               declare
                  Prefix_Seq : Unbounded_String := Prefix(Sequences.Element(J).Seq, K);
                  Suffix_Seq : Unbounded_String := Suffix(Sequences.Element(I).Seq, K);
               begin
                  if Prefix_Seq = Suffix_Seq then
                     Put_Line(To_String(Sequences.Element(I).ID) & " " &
                              To_String(Sequences.Element(J).ID));
                     Found_Edge := True;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      
      if not Found_Edge then
         Put_Line("No overlaps found");
      end if;
   end Print_Overlap_Graph;

   Sequences : Vector;
begin
   -- Read input sequences
   Read_Fasta(Sequences);
   
   -- Print overlap graph with k=3
   Print_Overlap_Graph(Sequences, 3);
end Overlap_Graphs;
```

## How it works

1. **Data Structure**: Uses a tagged record to store DNA sequence information (ID and sequence)
2. **Input Reading**: Parses FASTA format input where sequences are identified by '>' prefix
3. **Overlap Detection**: For each pair of sequences, checks if the suffix of length k of one matches the prefix of length k of another
4. **Output**: Prints pairs of IDs that have overlapping sequences

## Key Features

- Handles FASTA formatted input properly
- Uses generic vectors for dynamic sequence storage
- Implements prefix and suffix functions with proper boundary checking
- Correctly identifies directed edges in the overlap graph
- Uses Ada's strong typing system for safety

## Example Input Format
```
>Rosalind_0498
AAATAAA
>Rosalind_2391
AAATTTT
>Rosalind_2323
TTTTTTT
```

## Time Complexity
O(n² × k) where n is the number of sequences and k is the overlap length

## Space Complexity  
O(n × m) where n is the number of sequences and m is the average sequence length