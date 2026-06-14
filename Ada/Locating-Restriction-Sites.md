# Rosalind Problem: Locating Restriction Sites

## Problem Description
In this problem, we need to find all reverse complement palindromes of length between 4 and 12 in a given DNA sequence.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Character_Literals; use Ada.Character_Literals;

procedure Locating_Restriction_Sites is
   
   -- Function to compute reverse complement of a DNA string
   function Reverse_Complement(S : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;
      C : Character;
   begin
      for I in reverse 1..Length(S) loop
         C := Element(S, I);
         case C is
            when 'A' => Append(Result, 'T');
            when 'T' => Append(Result, 'A');
            when 'G' => Append(Result, 'C');
            when 'C' => Append(Result, 'G');
            when others => null;
         end case;
      end loop;
      return Result;
   end Reverse_Complement;
   
   -- Function to check if a string is palindrome
   function Is_Palindrome(S : Unbounded_String) return Boolean is
      Rev : Unbounded_String := Reverse_Complement(S);
   begin
      return S = Rev;
   end Is_Palindrome;
   
   -- Main program
   procedure Solve is
      DNA : Unbounded_String;
      Line : Unbounded_String;
      Start_Pos : Integer;
      Length_Val : Integer;
      Substring : Unbounded_String;
   begin
      -- Read the DNA sequence (skip the first line which contains FASTA header)
      Get_Line(Line);
      loop
         exit when End_Of_File;
         Get_Line(Line);
         if Line /= Null_Unbounded_String and then Element(Line, 1) /= '>' then
            Append(DNA, Line);
         end if;
      end loop;
      
      -- Find all palindromic restriction sites
      for Length_Val in 4..12 loop
         for Start_Pos in 1..(Length(DNA) - Length_Val + 1) loop
            Substring := Slice(DNA, Start_Pos, Start_Pos + Length_Val - 1);
            if Is_Palindrome(Substring) then
               Put_Line(Integer'Image(Start_Pos) & " " & Integer'Image(Length_Val));
            end if;
         end loop;
      end loop;
   end Solve;
   
begin
   Solve;
end Locating_Restriction_Sites;
```

## Explanation

1. **Reverse Complement Function**: 
   - Converts each nucleotide to its complement (A↔T, G↔C)
   - Reverses the resulting string to get the reverse complement

2. **Palindrome Check**:
   - Computes the reverse complement of a substring
   - Compares it with the original substring to check if they're equal

3. **Main Algorithm**:
   - Reads DNA sequence from input (skipping FASTA headers)
   - For each possible length from 4 to 12:
     - For each possible starting position:
       - Extracts a substring of that length
       - Checks if it's a palindrome
       - If so, outputs the position and length

## Sample Input Format
```
>Rosalind_0000
ATATCGATCGATCGATCGATCG
```

## Sample Output Format
```
1 4
3 4
5 4
...
```

The output shows positions (1-indexed) and lengths of all palindromic restriction sites found in the DNA sequence.