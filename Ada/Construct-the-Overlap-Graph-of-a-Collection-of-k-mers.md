# Rosalind Problem: Construct the Overlap Graph of a Collection of k-mers

## Problem Understanding

We need to construct an overlap graph from a collection of k-mers (strings of length k). In this graph:
- Each k-mer is a node
- There's a directed edge from k-mer A to k-mer B if the suffix of A (k-1 characters) matches the prefix of B (k-1 characters)
- We output all edges in the format "A -> B"

## Solution Approach

1. Parse input to get all k-mers
2. For each pair of k-mers, check if the suffix of one matches the prefix of another
3. Output all valid edges

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Construct_Overlap_Graph is
   
   package U_String_Vector is new Ada.Containers.Vectors (Positive, Unbounded_String);
   use U_String_Vector;
   
   -- Function to get the suffix of a string (all characters except first)
   function Suffix(S : Unbounded_String; Length : Natural) return Unbounded_String is
      Start_Index : constant Natural := Length - 1;
   begin
      if Start_Index >= Length then
         return To_Unbounded_String("");
      else
         return To_Unbounded_String(Slice(S, Start_Index + 1, Length));
      end if;
   end Suffix;
   
   -- Function to get the prefix of a string (all characters except last)
   function Prefix(S : Unbounded_String; Length : Natural) return Unbounded_String is
      End_Index : constant Natural := Length - 1;
   begin
      if End_Index <= 0 then
         return To_Unbounded_String("");
      else
         return To_Unbounded_String(Slice(S, 1, End_Index));
      end if;
   end Prefix;
   
   -- Function to check if suffix of A matches prefix of B
   function Overlaps(A, B : Unbounded_String; K : Natural) return Boolean is
      Suffix_A : constant Unbounded_String := Suffix(A, K);
      Prefix_B : constant Unbounded_String := Prefix(B, K);
   begin
      return Suffix_A = Prefix_B;
   end Overlaps;
   
   -- Read all k-mers from input
   K_Mers : Vector;
   Line   : Unbounded_String;
   K      : Natural;
   
begin
   -- Read the k value (assumes it's the first line)
   Get_Line(Line);
   K := Integer'Value(To_String(Line));
   
   -- Read all k-mers
   while not End_Of_File loop
      Get_Line(Line);
      if Length(Line) > 0 then
         Append(K_Mers, Line);
      end if;
   end loop;
   
   -- Check all pairs for overlaps
   for I in 1 .. Length(K_Mers) loop
      for J in 1 .. Length(K_Mers) loop
         -- Skip self-edges
         if I /= J then
            declare
               A : constant Unbounded_String := Element(K_Mers, I);
               B : constant Unbounded_String := Element(K_Mers, J);
            begin
               -- Check if suffix of A matches prefix of B
               if Overlaps(A, B, K - 1) then
                  Put_Line(To_String(Element(K_Mers, I)) & " -> " & To_String(Element(K_Mers, J)));
               end if;
            end;
         end if;
      end loop;
   end loop;
   
end Construct_Overlap_Graph;
```

## Example Usage

**Input:**
```
3
AAAT
AATG
ATGG
TGGT
GGTT
GTTT
TTTG
TTGT
```

**Output:**
```
AAAT -> AATG
AATG -> ATGG
ATGG -> TGGT
TGGT -> GGTT
GGTT -> GTTT
GTTT -> TTTG
TTTG -> TTGT
```

## Key Features

1. **Generic Vector Container**: Uses Ada's container library for dynamic storage of k-mers
2. **String Operations**: Implements custom suffix and prefix functions
3. **Overlap Detection**: Compares k-1 length suffix of one string with k-1 length prefix of another
4. **Edge Generation**: Outputs all valid directed edges in the required format

## Time Complexity
- O(n² × k) where n is the number of k-mers and k is the k-mer length
- For each pair of k-mers, we perform a string comparison of length k-1

## Space Complexity
- O(n × k) for storing the k-mers

This solution handles the overlap graph construction efficiently using Ada's strong typing and built-in container libraries.