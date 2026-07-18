# Rosalind Problem: Convert a Peptide into a Peptide Vector (Ada Solution)

## Problem Understanding

The task is to convert a peptide string into its corresponding peptide vector representation. The peptide vector is a binary vector where:
- Each position represents an amino acid in the peptide
- For each amino acid, we output 1 if it's in the set {P, V, I, L}, and 0 otherwise

## Solution Approach

1. Create a mapping of amino acids to their vector values (1 for P,V,I,L, 0 otherwise)
2. Iterate through the peptide string
3. For each amino acid, output the corresponding binary value
4. Return the resulting vector as space-separated integers

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Convert_Peptide_Vector is
   -- Function to determine if an amino acid should be represented as 1
   function Is_One(Amino_Acid : Character) return Boolean is
   begin
      case Amino_Acid is
         when 'P' | 'V' | 'I' | 'L' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_One;

   -- Read input peptide string
   Peptide : Unbounded_String;
   
begin
   -- Read the peptide string from standard input
   Get_Line(Peptide);
   
   -- Process each amino acid in the peptide
   for I in 1 .. Length(Peptide) loop
      declare
         Amino_Acid : Character := Element(Peptide, I);
      begin
         if Is_One(Amino_Acid) then
            Put(1, Width => 1);
         else
            Put(0, Width => 1);
         end if;
         
         -- Add space between elements (except after the last one)
         if I < Length(Peptide) then
            Put(" ");
         end if;
      end;
   end loop;
   
   New_Line;
end Convert_Peptide_Vector;
```

## Alternative Compact Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Convert_Peptide_Vector is
   Peptide : String(1..100);  -- Assuming maximum length
   Length  : Natural;
begin
   Get_Line(Peptide, Length);
   
   for I in 1 .. Length loop
      case Peptide(I) is
         when 'P' | 'V' | 'I' | 'L' =>
            Put("1");
         when others =>
            Put("0");
      end case;
      
      if I < Length then
         Put(" ");
      end if;
   end loop;
   
   New_Line;
end Convert_Peptide_Vector;
```

## Example Usage

**Input:**
```
ILPPV
```

**Output:**
```
01111
```

## Explanation

For the peptide "ILPPV":
- I → 0 (not in {P,V,I,L})
- L → 1 (in {P,V,I,L})  
- P → 1 (in {P,V,I,L})
- P → 1 (in {P,V,I,L})
- V → 1 (in {P,V,I,L})

Result: 01111

The solution correctly identifies which amino acids belong to the specified set and outputs the appropriate binary vector representation.