# Rosalind Problem: Find the Most Frequent Words in a String (Ada Solution)

## Problem Understanding

Given a DNA string and an integer k, we need to find all k-mers that appear most frequently in the string.

## Solution Approach

1. Generate all possible k-mers from the DNA string
2. Count the frequency of each k-mer
3. Find the maximum frequency
4. Return all k-mers with maximum frequency

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Find_The_Most_Frequent_Words is
   
   package String_Vector is new Ada.Containers.Vectors (Index_Type => Positive, 
                                                       Element_Type => Unbounded_String);
   
   package String_Map is new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                                          Element_Type => Natural);
   
   procedure Find_Most_Frequent_Words (DNA : Unbounded_String; k : Natural) is
      DNA_Str : constant String := To_String(DNA);
      Frequency_Map : String_Map.Map;
      Max_Count : Natural := 0;
      Most_Frequent : String_Vector.Vector;
   begin
      -- Handle edge cases
      if k = 0 or else DNA_Str'Length < k then
         Put_Line("No k-mers found");
         return;
      end if;
      
      -- Count frequency of each k-mer
      for i in 1 .. DNA_Str'Length - k + 1 loop
         declare
            K_Mer : constant Unbounded_String := To_Unbounded_String(DNA_Str(i..i+k-1));
         begin
            if String_Map.Contains(Frequency_Map, K_Mer) then
               declare
                  Count : Natural := String_Map.Element(Frequency_Map, K_Mer);
               begin
                  Count := Count + 1;
                  String_Map.Replace_Element(Frequency_Map, K_Mer, Count);
                  if Count > Max_Count then
                     Max_Count := Count;
                  end if;
               end;
            else
               String_Map.Insert(Frequency_Map, K_Mer, 1);
               if 1 > Max_Count then
                  Max_Count := 1;
               end if;
            end if;
         end;
      end loop;
      
      -- Collect all k-mers with maximum frequency
      for Element in String_Map.Iterate(Frequency_Map) loop
         if String_Map.Element(Element) = Max_Count then
            String_Vector.Append(Most_Frequent, String_Map.Key(Element));
         end if;
      end loop;
      
      -- Output results
      for I in 1 .. String_Vector.Length(Most_Frequent) loop
         Put_Line(To_String(String_Vector.Element(Most_Frequent, I)));
      end loop;
   end Find_Most_Frequent_Words;
   
begin
   -- Example usage
   declare
      DNA_String : constant Unbounded_String := To_Unbounded_String("ACGTTGCATGTCGCATGATGCATGAGAGCT");
      K_Value : constant Natural := 4;
   begin
      Find_Most_Frequent_Words(DNA_String, K_Value);
   end;
end Find_The_Most_Frequent_Words;
```

## Alternative Implementation (More Compact)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

procedure Find_The_Most_Frequent_Words is
   
   package String_Map is new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                                          Element_Type => Natural);
   
   procedure Solve (DNA : Unbounded_String; k : Natural) is
      DNA_Str : constant String := To_String(DNA);
      Frequency_Map : String_Map.Map;
      Max_Count : Natural := 0;
   begin
      -- Handle invalid inputs
      if k = 0 or else DNA_Str'Length < k then
         return;
      end if;
      
      -- Count k-mers
      for i in 1 .. DNA_Str'Length - k + 1 loop
         declare
            K_Mer : constant Unbounded_String := To_Unbounded_String(DNA_Str(i..i+k-1));
         begin
            if String_Map.Contains(Frequency_Map, K_Mer) then
               String_Map.Replace_Element(Frequency_Map, K_Mer, 
                                         String_Map.Element(Frequency_Map, K_Mer) + 1);
            else
               String_Map.Insert(Frequency_Map, K_Mer, 1);
            end if;
         end;
      end loop;
      
      -- Find maximum count
      for Element in String_Map.Iterate(Frequency_Map) loop
         Max_Count := Natural'Max(Max_Count, String_Map.Element(Element));
      end loop;
      
      -- Output results
      for Element in String_Map.Iterate(Frequency_Map) loop
         if String_Map.Element(Element) = Max_Count then
            Put_Line(To_String(String_Map.Key(Element)));
         end if;
      end loop;
   end Solve;
   
begin
   -- Example test case
   Solve(To_Unbounded_String("ACGTTGCATGTCGCATGATGCATGAGAGCT"), 4);
end Find_The_Most_Frequent_Words;
```

## Key Features of the Solution

1. **Use of Unbounded Strings**: Handles DNA sequences of any length
2. **Ordered Maps**: Efficiently stores and retrieves k-mer frequencies
3. **Proper Error Handling**: Checks for valid input parameters
4. **Memory Efficiency**: Only stores necessary information
5. **Clear Logic Flow**: Steps clearly separated for easy understanding

## Expected Output for Example Input
For DNA string "ACGTTGCATGTCGCATGATGCATGAGAGCT" with k=4:
```
CATG
GCAT
```

The solution correctly identifies the most frequent 4-mers in the given DNA sequence.