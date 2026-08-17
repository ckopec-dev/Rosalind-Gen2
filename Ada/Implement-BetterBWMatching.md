# Rosalind Problem: Implement_BetterBWMatching in Ada

## Problem Understanding

The BetterBWMatching algorithm is an optimized version of the Burrows-Wheeler Matching algorithm that uses precomputed data structures to efficiently find occurrences of patterns in a BWT string.

## Solution Approach

We'll implement:
1. Preprocessing step to build count and first occurrence arrays
2. The BetterBWMatching function that finds pattern occurrences

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Implement_BetterBWMatching is
   
   -- Type definitions
   type BWT_String is array (Positive range <>) of Character;
   type Count_Array is array (Character, Positive range <>) of Natural;
   type First_Occurrence_Array is array (Character) of Natural;
   
   -- Global variables for preprocessing
   BWT : BWT_String(1..0);
   Count : Count_Array('A'..'Z', 1..0);
   First_Occurrence : First_Occurrence_Array('A'..'Z');
   
   -- Function to preprocess BWT string
   procedure Preprocess_BWT(BWT_String : in String) is
      Alphabet : array ('A'..'Z') of Natural := (others => 0);
      Max_Pos : Natural;
   begin
      -- Resize arrays based on input length
      BWT := BWT_String(1..BWT_String'Length);
      
      -- Initialize Count array to zeros
      for Ch in Character loop
         for Pos in 1..BWT'Length loop
            Count(Ch, Pos) := 0;
         end loop;
      end loop;
      
      -- Build count matrix
      for i in 1..BWT'Length loop
         -- Update counts for all characters up to position i
         for Ch in Character loop
            Count(Ch, i) := Count(Ch, i-1);
         end loop;
         
         -- Increment count for current character
         Count(BWT(i), i) := Count(BWT(i), i) + 1;
      end loop;
      
      -- Build first occurrence array
      First_Occurrence('A') := 0;
      Max_Pos := BWT'Length;
      
      -- Find first occurrence positions of each character
      for Ch in Character loop
         First_Occurrence(Ch) := Natural'Last;
      end loop;
      
      -- Find first occurrences from right to left
      for i in reverse BWT'Range loop
         if First_Occurrence(BWT(i)) = Natural'Last then
            First_Occurrence(BWT(i)) := i - 1;
         end if;
      end loop;
      
      -- Adjust positions so that 'A' starts at position 0
      for Ch in Character loop
         if Ch /= 'A' and First_Occurrence(Ch) = Natural'Last then
            First_Occurrence(Ch) := Max_Pos;
         end if;
      end loop;
   end Preprocess_BWT;
   
   -- BetterBWMatching function
   function BetterBWMatching(BWT : in String; Pattern : in String) return Natural is
      Text_Length : constant Natural := BWT'Length;
      Top, Bottom : Natural;
      Last_Ch : Character;
      Found : Boolean := True;
   begin
      -- Initialize search range
      Top := 0;
      Bottom := Text_Length - 1;
      
      -- Process pattern from right to left
      for i in reverse Pattern'Range loop
         Last_Ch := Pattern(i);
         
         -- Check if character exists in BWT
         if First_Occurrence(Last_Ch) = Natural'Last then
            Found := False;
            exit;
         end if;
         
         -- Calculate new Top and Bottom
         Top := First_Occurrence(Last_Ch) + 
                (if Top > 0 then Count(Last_Ch, Top-1) else 0);
         Bottom := First_Occurrence(Last_Ch) + 
                   Count(Last_Ch, Bottom) - 1;
         
         -- If Top > Bottom, no matches
         if Top > Bottom then
            Found := False;
            exit;
         end if;
      end loop;
      
      if not Found then
         return 0;
      else
         return Bottom - Top + 1;
      end if;
   end BetterBWMatching;
   
   -- Read input from stdin
   procedure Read_Input is
      Input_Line : Unbounded_String;
      BWT_String : String(1..1000);
      Pattern : String(1..1000);
      BWT_Length, Pattern_Length : Natural;
   begin
      -- Read BWT string
      Get_Line(Input_Line);
      BWT_String := To_String(Input_Line);
      BWT_Length := BWT_String'Length;
      
      -- Read pattern (may be multiple patterns on next line)
      Get_Line(Input_Line);
      Pattern := To_String(Input_Line);
      Pattern_Length := Pattern'Length;
      
      -- Preprocess BWT
      Preprocess_BWT(BWT_String);
      
      -- Find matches for pattern
      Put_Line(Integer'Image(BetterBWMatching(BWT_String, Pattern)));
   end Read_Input;
   
begin
   Read_Input;
end Implement_BetterBWMatching;
```

## Key Features of Implementation

1. **Preprocessing Phase**: Builds count matrix and first occurrence array
2. **BetterBWMatching Algorithm**: Uses the precomputed data structures for efficient pattern matching
3. **Character Handling**: Properly handles all characters in the alphabet (A-Z)
4. **Boundary Conditions**: Handles cases where patterns don't match any text

## Time Complexity

- **Preprocessing**: O(n) where n is the length of BWT string
- **Pattern Matching**: O(m) where m is the length of pattern
- **Overall**: O(n + m)

## Space Complexity

O(n) for storing count matrix and first occurrence array

This implementation efficiently solves the BetterBWMatching problem by leveraging precomputed data structures to avoid repeated calculations during pattern matching.