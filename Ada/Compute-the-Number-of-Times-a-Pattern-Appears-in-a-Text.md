# Rosalind Problem: Compute the Number of Times a Pattern Appears in a Text (Ada Solution)

## Problem Understanding

We need to count how many times a pattern appears in a text, including overlapping occurrences.

## Solution Approach

1. Read the pattern and text from input
2. Use a sliding window approach to check each possible position
3. Count all matches (including overlapping ones)
4. Return the total count

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Compute_Pattern_Count is
   -- Function to count pattern occurrences in text
   function Count_Patterns(Text : Unbounded_String; Pattern : Unbounded_String) return Natural is
      Text_Length : constant Natural := Length(Text);
      Pattern_Length : constant Natural := Length(Pattern);
      Count : Natural := 0;
      
      -- Helper function to check if pattern matches at position
      function Matches_At(Position : Natural) return Boolean is
         I : Natural;
      begin
         if Position + Pattern_Length > Text_Length then
            return False;
         end if;
         
         for I in 1..Pattern_Length loop
            if Element(Text, Position + I) /= Element(Pattern, I) then
               return False;
            end if;
         end loop;
         return True;
      end Matches_At;
   begin
      -- Check each possible position in text
      for Pos in 1..Text_Length - Pattern_Length + 1 loop
         if Matches_At(Pos) then
            Count := Count + 1;
         end if;
      end loop;
      
      return Count;
   end Count_Patterns;
   
   -- Read input lines
   Text_Line : Unbounded_String;
   Pattern_Line : Unbounded_String;
   Pattern_Count : Natural;
   
begin
   -- Read pattern and text
   Text_Line := To_Unbounded_String(Get_Line);
   Pattern_Line := To_Unbounded_String(Get_Line);
   
   -- Count patterns
   Pattern_Count := Count_Patterns(Text_Line, Pattern_Line);
   
   -- Output result
   Put_Line(Natural'Image(Pattern_Count));
end Compute_Pattern_Count;
```

## How It Works

1. **Input Reading**: The program reads two lines - first line is the text, second line is the pattern
2. **Pattern Matching Function**: `Count_Patterns` function:
   - Iterates through each possible starting position in the text
   - For each position, checks if the pattern matches at that location
   - Counts all matches including overlapping ones
3. **Overlapping Handling**: The algorithm checks every position (not just non-overlapping), so overlapping patterns are counted correctly

## Example Usage

Input:
```
GATATATGCATATACTT
ATAT
```

Output:
```
3
```

The pattern "ATAT" appears at positions 2, 4, and 10 in the text "GATATATGCATATACTT".

## Time Complexity
- **Time**: O(n × m) where n is the length of text and m is the length of pattern
- **Space**: O(1) additional space (not counting input storage)

## Key Features

- Handles overlapping patterns correctly
- Uses Ada's string handling capabilities with `Unbounded_String`
- Efficient sliding window approach
- Clear separation of concerns with helper functions
- Proper input/output handling for Rosalind format