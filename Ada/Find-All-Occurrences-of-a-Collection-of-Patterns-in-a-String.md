# Rosalind Problem: Find All Occurrences of a Collection of Patterns in a String

## Problem Description
Given a string `s` and a collection of patterns, find all occurrences of each pattern in the string.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Find_All_Occurrences_of_a_Collection_of_Patterns_in_a_String is
   
   type Pattern_Array is array (Positive range <>) of Unbounded_String;
   
   function Find_Occurrences(Text : Unbounded_String; 
                            Pattern : Unbounded_String) return Integer_Array
   is
      Positions : Integer_Array(1..Length(Text)) := (others => 0);
      Count : Integer := 0;
      Text_Str : constant String := To_String(Text);
      Pattern_Str : constant String := To_String(Pattern);
      Text_Len : constant Integer := Length(Text);
      Pattern_Len : constant Integer := Length(Pattern);
   begin
      if Pattern_Len > Text_Len then
         return (1..0 => 0);
      end if;
      
      for I in 1..Text_Len - Pattern_Len + 1 loop
         if Text_Str(I..I + Pattern_Len - 1) = Pattern_Str then
            Count := Count + 1;
            Positions(Count) := I;
         end if;
      end loop;
      
      return Positions(1..Count);
   end Find_Occurrences;
   
   procedure Print_Positions(Positions : Integer_Array) is
   begin
      for I in Positions'First..Positions'Last loop
         Put(Positions(I), 0);
         if I < Positions'Last then
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Print_Positions;
   
   -- Main program
   Text : Unbounded_String := To_Unbounded_String("ATATATATAT");
   Patterns : Pattern_Array(1..3) := 
     (To_Unbounded_String("ATA"), 
      To_Unbounded_String("AT"), 
      To_Unbounded_String("T"));
      
begin
   for I in Patterns'First..Patterns'Last loop
      declare
         Occurrences : constant Integer_Array := Find_Occurrences(Text, Patterns(I));
      begin
         Print_Positions(Occurrences);
      end;
   end loop;
end Find_All_Occurrences_of_a_Collection_of_Patterns_in_a_String;
```

## Explanation

This Ada solution works as follows:

1. **Data Types**: 
   - Uses `Unbounded_String` for flexible string handling
   - Defines `Pattern_Array` to store multiple patterns
   - Uses `Integer_Array` to store positions of matches

2. **Find_Occurrences Function**:
   - Takes a text string and pattern string as input
   - Returns an array of positions where the pattern occurs in the text
   - Handles edge cases (pattern longer than text)
   - Uses substring comparison to find matches

3. **Print_Positions Procedure**:
   - Outputs the found positions separated by spaces
   - Handles empty results properly

4. **Main Program**:
   - Demonstrates usage with sample data
   - Processes multiple patterns against a single text string
   - Outputs positions for each pattern separately

## Sample Input/Output

For input text "ATATATATAT" and patterns ["ATA", "AT", "T"]:
- Pattern "ATA": Positions 1, 3, 5, 7 (1-indexed)
- Pattern "AT": Positions 1, 3, 5, 7, 9 
- Pattern "T": Positions 2, 4, 6, 8, 10

## Key Ada Features Used

- **Unbounded Strings**: For flexible string manipulation
- **Arrays**: Both fixed and dynamic arrays for data storage
- **Procedures and Functions**: Modular code organization
- **Exception Handling**: Basic edge case management
- **String Operations**: Substring comparison using standard operations

The solution handles the core requirements of finding all occurrences of patterns in a string while following Ada's strong typing and structured programming principles.