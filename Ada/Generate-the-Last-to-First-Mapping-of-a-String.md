# Rosalind Problem: Last-to-First Mapping

I'll solve this step by step, implementing the Last-to-First mapping algorithm in Ada.

## Problem Understanding

The last-to-first mapping is used in BWT (Burrows-Wheeler Transform) where we need to map positions from the last column of the BWT matrix back to positions in the first column.

## Solution Approach

1. Create a function to compute the last-to-first mapping
2. For each position in the last column, find its corresponding position in the first column
3. Handle duplicate characters by tracking their order

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Last_To_First is
   
   type Char_Array is array (Positive range <>) of Character;
   type Int_Array is array (Positive range <>) of Integer;
   
   -- Function to create the first-to-last mapping
   function Create_Last_To_First_Map(Text : String) return Int_Array is
      Length : constant Integer := Text'Length;
      First_Column : Char_Array(1..Length);
      Last_Column : Char_Array(1..Length);
      Sorted_Column : Char_Array(1..Length);
      Count : array (Character) of Integer;
      Position : array (Character) of Integer;
      Result : Int_Array(1..Length);
      
   begin
      -- Initialize character counts
      for C in Character loop
         Count(C) := 0;
      end loop;
      
      -- Fill first and last columns
      for I in 1..Length loop
         First_Column(I) := Text(I);
         Last_Column(I) := Text(I);
      end loop;
      
      -- Sort first column to get sorted version
      Sorted_Column := First_Column;
      for I in 1..Length-1 loop
         for J in I+1..Length loop
            if Sorted_Column(J) < Sorted_Column(I) then
               declare
                  Temp : Character := Sorted_Column(I);
               begin
                  Sorted_Column(I) := Sorted_Column(J);
                  Sorted_Column(J) := Temp;
               end;
            end if;
         end loop;
      end loop;
      
      -- Create mapping from first to last positions
      for I in 1..Length loop
         Position(First_Column(I)) := Position(First_Column(I)) + 1;
      end loop;
      
      -- Reset position counters for mapping
      for C in Character loop
         Position(C) := 0;
      end loop;
      
      -- Create last-to-first mapping
      for I in 1..Length loop
         declare
            C : constant Character := Last_Column(I);
            P : Integer := Position(C);
         begin
            Result(I) := P + 1;
            Position(C) := P + 1;
         end;
      end loop;
      
      return Result;
   end Create_Last_To_First_Map;
   
   -- Alternative simpler approach for BWT mapping
   function Last_To_First_Position(First : String; Last : String; Pos : Integer) return Integer is
      Length : constant Integer := First'Length;
      First_Sorted : String(1..Length);
      Char_Count : array (Character) of Integer;
      Char_Rank : array (Character range 'A'..'Z') of Integer;
      
   begin
      -- Initialize counters
      for C in Character loop
         Char_Count(C) := 0;
      end loop;
      
      -- Count characters in first column
      for I in 1..Length loop
         Char_Count(First(I)) := Char_Count(First(I)) + 1;
      end loop;
      
      -- Create cumulative counts for rank calculation
      declare
         Cumulative : array (Character) of Integer := (others => 0);
         Total : Integer := 0;
      begin
         for C in Character loop
            if C >= 'A' and C <= 'Z' then
               Cumulative(C) := Total;
               Total := Total + Char_Count(C);
            end if;
         end loop;
         
         -- For each character, assign its rank
         for C in Character loop
            Char_Rank(C) := Cumulative(C);
         end loop;
      end;
      
      -- Find the character at position Pos in last column
      declare
         C : constant Character := Last(Pos);
      begin
         -- Return the corresponding position in first column
         return Char_Rank(C) + 1;
      end;
   end Last_To_First_Position;
   
   -- Main algorithm for complete mapping
   function Compute_Last_To_First(First : String; Last : String) return Int_Array is
      Length : constant Integer := First'Length;
      Result : Int_Array(1..Length);
      First_Sorted : String(1..Length);
      Char_Count : array (Character) of Integer;
      Cumulative : array (Character) of Integer;
      
   begin
      -- Initialize character counts
      for C in Character loop
         Char_Count(C) := 0;
      end loop;
      
      -- Count characters
      for I in 1..Length loop
         Char_Count(First(I)) := Char_Count(First(I)) + 1;
      end loop;
      
      -- Compute cumulative counts (prefix sums)
      declare
         Total : Integer := 0;
      begin
         for C in Character loop
            if C >= 'A' and C <= 'Z' then
               Cumulative(C) := Total;
               Total := Total + Char_Count(C);
            end if;
         end loop;
      end;
      
      -- Build result array - mapping from last column to first column
      for I in 1..Length loop
         declare
            C : constant Character := Last(I);
            Rank : Integer := Cumulative(C) + 1;
         begin
            Result(I) := Rank;
            Cumulative(C) := Cumulative(C) + 1;
         end;
      end loop;
      
      return Result;
   end Compute_Last_To_First;
   
   -- Read input and process
   procedure Process_Input is
      First_Column : String(1..200);
      Last_Column : String(1..200);
      Length : Integer;
      Result : Int_Array(1..200);
      Line : Unbounded_String;
   begin
      Put_Line("Enter the first column:");
      Get_Line(First_Column, Length);
      
      Put_Line("Enter the last column:");
      Get_Line(Last_Column, Length);
      
      -- Compute mapping
      Result := Compute_Last_To_First(First_Column, Last_Column);
      
      -- Output result
      Put_Line("Last-to-First mapping:");
      for I in 1..Length loop
         Put(Result(I), Width => 4);
      end loop;
      New_Line;
   end Process_Input;

begin
   Process_Input;
end Last_To_First;
```

## Explanation

The algorithm works by:

1. **Character Counting**: First, we count how many times each character appears in the first column.

2. **Cumulative Counting**: We calculate cumulative counts to determine where each character should be placed in the sorted first column.

3. **Mapping Construction**: For each position in the last column:
   - Find the character at that position
   - Use the cumulative count to find its corresponding position in the sorted first column
   - Update the cumulative count for that character

## Example Usage

If we have:
- First column: "ABCD"
- Last column: "DABC"

The mapping would be:
- Position 1 in last column (character 'D') maps to position 4 in first column
- Position 2 in last column (character 'A') maps to position 1 in first column  
- Position 3 in last column (character 'B') maps to position 2 in first column
- Position 4 in last column (character 'C') maps to position 3 in first column

## Time Complexity
O(n) where n is the length of the string, since we only need to traverse the strings once.

This implementation handles the core BWT last-to-first mapping functionality required for the Rosalind problem.