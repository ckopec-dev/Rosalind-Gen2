# Rosalind Problem: Find the Reverse Complement of a String (Ada Solution)

## Problem Understanding

The task is to find the reverse complement of a DNA string:
- Reverse the string
- Replace each nucleotide with its complement (A↔T, C↔G)

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Reverse_Complement is
   function Complement(Nucleotide : Character) return Character is
   begin
      case Nucleotide is
         when 'A' => return 'T';
         when 'T' => return 'A';
         when 'C' => return 'G';
         when 'G' => return 'C';
         when others => return Nucleotide;  -- Handle invalid characters
      end case;
   end Complement;

   function Reverse_Complement_String(S : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;
      Length : constant Natural := Length(S);
   begin
      -- Build reverse complement by processing from end to beginning
      for I in reverse 1..Length loop
         Append(Result, Complement(Element(S, I)));
      end loop;
      
      return Result;
   end Reverse_Complement_String;

   -- Read input string
   Input_Line : Unbounded_String := Null_Unbounded_String;
begin
   -- Read the DNA string from standard input
   Get_Line(Input_Line);
   
   -- Compute and output reverse complement
   Put_Line(Reverse_Complement_String(Input_Line));
end Reverse_Complement;
```

## How It Works

1. **Complement Function**: Maps each nucleotide to its complement:
   - A → T
   - T → A  
   - C → G
   - G → C

2. **Reverse_Complement_String Function**: 
   - Takes the input string and processes it from right to left
   - For each character, finds its complement using the `Complement` function
   - Builds the result string in reverse order

3. **Main Procedure**:
   - Reads a DNA string from standard input
   - Calls the reverse complement function
   - Outputs the result

## Example

Input: `AAAACCCGGT`
- Reverse: `TGGCCCAAAA`
- Complement: `ACCGGTTTTA`

Output: `ACCGGTTTTA`

## Compilation and Execution

```bash
gnatmake reverse_complement.adb
./reverse_complement
```

Then input the DNA string when prompted.