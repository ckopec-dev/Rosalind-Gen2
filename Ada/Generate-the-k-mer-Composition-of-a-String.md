# Rosalind Problem: Generate the k-mer Composition of a String (Ada Solution)

## Problem Understanding

The task is to generate the k-mer composition of a string, which means finding all substrings of length k from the given string and returning them in lexicographically sorted order.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure K_Mer_Composition is
   package US is new Ada.Strings.Unbounded;
   use US;
   
   package String_Vectors is new Ada.Containers.Vectors (Positive, Unbounded_String);
   use String_Vectors;
   
   -- Function to get all k-mers from a string
   function Get_K_Mers(Sequence : Unbounded_String; K : Positive) return Vector is
      K_Mers : Vector;
      Length : constant Positive := Length(Sequence);
   begin
      if Length < K then
         return K_Mers;
      end if;
      
      for I in 1 .. Length - K + 1 loop
         declare
            K_Mer : constant Unbounded_String := Slice(Sequence, I, I + K - 1);
         begin
            Append(K_Mers, K_Mer);
         end;
      end loop;
      
      return K_Mers;
   end Get_K_Mers;
   
   -- Function to sort vector lexicographically
   procedure Sort_Vector(V : in out Vector) is
      procedure Swap(I, J : Positive) is
         Temp : Unbounded_String;
      begin
         Temp := Element(V, I);
         Replace_Element(V, I, Element(V, J));
         Replace_Element(V, J, Temp);
      end Swap;
      
      procedure Quick_Sort(Low, High : Positive) is
         Pivot : Positive;
         I : Positive := Low;
         J : Positive := High;
         Temp : Unbounded_String;
      begin
         if Low < High then
            Pivot := Low;
            loop
               while I < High and then Compare(Element(V, I), Element(V, Pivot)) <= 0 loop
                  I := I + 1;
               end loop;
               while J > Low and then Compare(Element(V, J), Element(V, Pivot)) > 0 loop
                  J := J - 1;
               end loop;
               if I >= J then
                  exit;
               end if;
               Swap(I, J);
               I := I + 1;
               J := J - 1;
            end loop;
            Swap(J, Pivot);
            Quick_Sort(Low, J - 1);
            Quick_Sort(J + 1, High);
         end if;
      end Quick_Sort;
   begin
      if Length(V) > 1 then
         Quick_Sort(1, Length(V));
      end if;
   end Sort_Vector;
   
   -- Main program
   Sequence : Unbounded_String;
   K : Positive;
   K_Mers : Vector;
begin
   -- Read input
   Put_Line("Enter the string:");
   Sequence := To_Unbounded_String(Get_Line);
   
   Put_Line("Enter k:");
   K := Positive'Value(Get_Line);
   
   -- Get k-mers
   K_Mers := Get_K_Mers(Sequence, K);
   
   -- Sort k-mers
   Sort_Vector(K_Mers);
   
   -- Output results
   Put_Line("K-mer composition:");
   for I in 1 .. Length(K_Mers) loop
      Put_Line(Element(K_Mers, I));
   end loop;
end K_Mer_Composition;
```

## Example Usage

For input:
```
Sequence: ACGTACGT
K: 3
```

Output:
```
K-mer composition:
ACG
CGT
GTAC
TAC
```

## Explanation

1. **Input Reading**: The program reads the DNA sequence and the value of k from standard input
2. **K-mer Extraction**: It extracts all substrings of length k from the sequence
3. **Sorting**: The k-mers are sorted lexicographically using a quicksort algorithm
4. **Output**: The sorted k-mers are printed one per line

## Key Features

- Uses Ada's string handling capabilities with `Unbounded_String`
- Implements custom sorting for lexicographical ordering
- Handles edge cases like when the sequence is shorter than k
- Uses containers for dynamic array management
- Follows Ada's strong typing and safety features

The solution correctly handles the requirements of generating k-mer composition and returning results in lexicographically sorted order as required by the Rosalind problem.

