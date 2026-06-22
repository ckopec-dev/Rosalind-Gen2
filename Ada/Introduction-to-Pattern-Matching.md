# Rosalind Problem: Introduction to Pattern Matching

## Problem Description

Given two strings `s` and `t`, find all positions where `t` occurs as a substring in `s`.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Introduction_To_Pattern_Matching is
   type String_Access is access all String;
   
   -- Function to find all occurrences of pattern in text
   function Find_All_Positions(Text : String; Pattern : String) return String is
      Positions : String(1..1000);
      Pos_Count : Integer := 0;
      Text_Length : constant Integer := Text'Length;
      Pattern_Length : constant Integer := Pattern'Length;
   begin
      if Pattern_Length > Text_Length then
         return "";
      end if;
      
      for I in 1..Text_Length - Pattern_Length + 1 loop
         if Text(I..I + Pattern_Length - 1) = Pattern then
            Pos_Count := Pos_Count + 1;
            if Pos_Count = 1 then
               Positions(Pos_Count) := Character'Val(0);
            else
               Positions(Pos_Count) := ' ';
            end if;
            -- Convert position to 1-based indexing and add to result
            declare
               Pos_Str : String(1..10);
               Pos_Int : constant Integer := I;
            begin
               Pos_Str := Integer'Image(Pos_Int);
               for J in 1..Pos_Str'Length loop
                  if Pos_Str(J) /= ' ' then
                     Positions(Pos_Count + J - 1) := Pos_Str(J);
                  end if;
               end loop;
               Pos_Count := Pos_Count + Pos_Str'Length - 1;
            end;
         end if;
      end loop;
      
      if Pos_Count = 0 then
         return "";
      else
         return Positions(1..Pos_Count);
      end if;
   end Find_All_Positions;
   
   -- More efficient approach using direct string comparison
   function Find_Pattern_Positions(Text : String; Pattern : String) return String is
      Result : String := "";
      Text_Length : constant Integer := Text'Length;
      Pattern_Length : constant Integer := Pattern'Length;
   begin
      if Pattern_Length > Text_Length then
         return "";
      end if;
      
      for I in 1..Text_Length - Pattern_Length + 1 loop
         if Text(I..I + Pattern_Length - 1) = Pattern then
            if Result'Length > 0 then
               Result := Result & " ";
            end if;
            Result := Result & Integer'Image(I);
         end if;
      end loop;
      
      return Result;
   end Find_Pattern_Positions;
   
begin
   -- Read input from stdin (assuming standard input)
   declare
      Text : String(1..1000);
      Pattern : String(1..1000);
      Text_Length, Pattern_Length : Integer;
      Input_Line : String(1..1000);
      Line_Length : Integer;
   begin
      -- Read text (first line)
      Get_Line(Input_Line, Line_Length);
      Text_Length := Line_Length;
      for I in 1..Line_Length loop
         Text(I) := Input_Line(I);
      end loop;
      
      -- Read pattern (second line)
      Get_Line(Input_Line, Line_Length);
      Pattern_Length := Line_Length;
      for I in 1..Line_Length loop
         Pattern(I) := Input_Line(I);
      end loop;
      
      -- Find and print positions
      declare
         Positions : String := Find_Pattern_Positions(Text(1..Text_Length), Pattern(1..Pattern_Length));
      begin
         if Positions'Length > 0 then
            Put_Line(Positions);
         end if;
      end;
   end;
end Introduction_To_Pattern_Matching;
```

## Alternative Simpler Approach

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Pattern_Matching is
   Text : String(1..1000);
   Pattern : String(1..1000);
   Text_Length, Pattern_Length : Integer;
   
begin
   -- Read the text (first line)
   Get_Line(Text, Text_Length);
   
   -- Read the pattern (second line)
   Get_Line(Pattern, Pattern_Length);
   
   -- Find all occurrences of pattern in text
   for I in 1..Text_Length - Pattern_Length + 1 loop
      if Text(I..I + Pattern_Length - 1) = Pattern then
         Put(Integer'Image(I));  -- Print position (1-indexed)
         Put(" ");
      end if;
   end loop;
   
   New_Line;
end Pattern_Matching;
```

## Explanation

This Ada solution:

1. **Reads input**: Takes two strings from standard input - the text and pattern
2. **Searches for matches**: Uses a loop to check every possible position in the text where the pattern could occur
3. **Handles indexing**: Uses 1-based indexing as required by Rosalind problems
4. **Outputs results**: Prints all matching positions separated by spaces

## Key Features

- **Input handling**: Properly reads strings from standard input
- **Boundary checking**: Ensures we don't go beyond string bounds
- **Position reporting**: Outputs 1-indexed positions as required
- **Efficient search**: O(n*m) time complexity where n is text length and m is pattern length

## Sample Input/Output

**Input:**
```
ATATATATATATATAT
TATAT
```

**Output:**
```
2 4 6 8 10 12 14
```