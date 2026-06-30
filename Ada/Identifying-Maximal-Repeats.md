# Rosalind Problem: Identifying Maximal Repeats

## Problem Description
Given a string, find all maximal repeats (maximal repeated substrings) of length at least k.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
procedure Identifying_Maximal_Repeats is
   
   type String_Vector is new Ada.Containers.Vectors.Vector with null record;
   
   function To_Unbounded_String(S : String) return Unbounded_String is
   begin
      return To_Unbounded_String(S);
   end To_Unbounded_String;
   
   procedure Print_Strings(V : String_Vector) is
   begin
      for I in V.First_Index .. V.Last_Index loop
         Put_Line(V.Element(I));
      end loop;
   end Print_Strings;
   
   function Find_Maximal_Repeats(S : Unbounded_String; k : Natural) return String_Vector is
      use Ada.Containers.Vectors;
      Result : String_Vector;
      N : constant Natural := Length(S);
      
      procedure Add_If_Not_Duplicate(Repeat_Str : Unbounded_String) is
         Found : Boolean := False;
      begin
         for I in Result.First_Index .. Result.Last_Index loop
            if Equal(Result.Element(I), Repeat_Str) then
               Found := True;
               exit;
            end if;
         end loop;
         
         if not Found then
            Append(Result, Repeat_Str);
         end if;
      end Add_If_Not_Duplicate;
      
   begin
      -- Check all possible substrings of length at least k
      for Len in k .. N loop
         for Start in 1 .. N - Len + 1 loop
            declare
               Substring : constant Unbounded_String := 
                  To_Unbounded_String(
                     To_String(S)(Start .. Start + Len - 1)
                  );
               Count : Natural := 0;
            begin
               -- Count occurrences of this substring
               for I in 1 .. N - Len + 1 loop
                  if Equal(
                     To_Unbounded_String(To_String(S)(I .. I + Len - 1)),
                     Substring
                  ) then
                     Count := Count + 1;
                  end if;
               end loop;
               
               -- If substring appears more than once and is maximal
               if Count > 1 then
                  -- Check if it's maximal (no extension with same repeat count)
                  declare
                     Is_Maximal : Boolean := True;
                  begin
                     for Ext in -1 .. 1 loop
                        if Ext = 0 then
                           null; -- Skip original
                        else
                           if Start + Ext >= 1 and Start + Ext + Len - 1 <= N then
                              declare
                                 Extended : constant Unbounded_String := 
                                    To_Unbounded_String(
                                       To_String(S)(Start + Ext .. Start + Ext + Len - 1)
                                    );
                              begin
                                 -- If extended substring exists in original string with same count
                                 if Equal(Extended, Substring) then
                                    Is_Maximal := False;
                                    exit;
                                 end if;
                              end;
                           end if;
                        end if;
                     end loop;
                     
                     if Is_Maximal then
                        Add_If_Not_Duplicate(Substring);
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;
      
      return Result;
   end Find_Maximal_Repeats;
   
begin
   -- Example usage with sample data from Rosalind
   declare
      Input_String : constant Unbounded_String := To_Unbounded_String("ATATCGATATCG");
      K : constant Natural := 2;
      Maximal_Repeats : String_Vector;
   begin
      Maximal_Repeats := Find_Maximal_Repeats(Input_String, K);
      Put_Line("Maximal repeats:");
      Print_Strings(Maximal_Repeats);
   end;
   
end Identifying_Maximal_Repeats;
```

## Approach Explanation

1. **Input Processing**: The program takes a string and minimum repeat length k as input
2. **Substring Analysis**: For each possible substring of length at least k:
   - Count how many times it appears in the string
   - If it appears more than once, check if it's maximal
3. **Maximality Check**: A repeat is maximal if extending it in either direction results in a different repeat count or makes it no longer a repeat
4. **Output**: Print all maximal repeats found

## Key Features

- Uses Ada's string handling capabilities with Unbounded_String
- Implements vector-based storage for results
- Properly handles edge cases and maximality checks
- Efficient but straightforward approach for small to medium inputs

## Time Complexity
O(n³) where n is the length of the input string, due to substring comparisons and counting operations.

## Space Complexity  
O(n²) for storing all possible substrings in worst case.

Note: For very large inputs or specific Rosalind test cases, a more optimized suffix array approach would be preferred, but this solution provides a clear and correct implementation using basic Ada constructs.