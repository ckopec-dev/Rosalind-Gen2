# Rosalind Problem: Identifying Reversing Substitutions

## Problem Description
Given two DNA strings of equal length, determine the number of reversing substitutions (also known as transversions) between them.

A reversing substitution occurs when a DNA base is replaced by a base from the opposite purine/pyrimidine group:
- A ↔ G (purine to purine, but this is actually a transition)
- C ↔ T (pyrimidine to pyrimidine, but this is actually a transition)

Wait, let me correct this - a reversing substitution (transversion) is when:
- A ↔ C or A ↔ T (purine to pyrimidine)
- G ↔ C or G ↔ T (purine to pyrimidine)
- C ↔ A or C ↔ G (pyrimidine to purine)  
- T ↔ A or T ↔ G (pyrimidine to purine)

Actually, let me re-read this more carefully. Looking at the typical definition:
- Transitions: A↔G, C↔T (same purine/pyrimidine group)
- Transversions: A↔C, A↔T, G↔C, G↔T (different purine/pyrimidine groups)

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Identifying_Reversing_Substitutions is
   type DNA_Base is ('A', 'C', 'G', 'T');
   
   function Is_Transversion(Base1, Base2 : DNA_Base) return Boolean is
   begin
      if Base1 = Base2 then
         return False;
      end if;
      
      -- Check if they are transitions (same purine/pyrimidine group)
      if (Base1 = 'A' and Base2 = 'G') or (Base1 = 'G' and Base2 = 'A') or
         (Base1 = 'C' and Base2 = 'T') or (Base1 = 'T' and Base2 = 'C') then
         return False;  -- This is a transition, not transversion
      else
         return True;   -- This is a transversion
      end if;
   end Is_Transversion;
   
   function Count_Transversions(String1, String2 : String) return Natural is
      Count : Natural := 0;
   begin
      for I in String1'Range loop
         if Is_Transversion(DNA_Base(String1(I)), DNA_Base(String2(I))) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Transversions;
   
   -- Read input from standard input
   Line1 : Unbounded_String;
   Line2 : Unbounded_String;
   Result : Natural;
   
begin
   -- Read two DNA strings
   Get_Line(Line1);
   Get_Line(Line2);
   
   -- Convert to strings and count transversions
   Result := Count_Transversions(To_String(Line1), To_String(Line2));
   
   -- Output result
   Put_Line(Integer'Image(Result));
end Identifying_Reversing_Substitutions;
```

## Explanation

This Ada solution works as follows:

1. **Type Definition**: Defines a `DNA_Base` type with the four DNA nucleotides: A, C, G, T.

2. **Is_Transversion Function**: 
   - Takes two DNA bases as input
   - Returns `False` if they are identical
   - Returns `False` if they are transitions (A↔G or C↔T - same purine/pyrimidine group)
   - Returns `True` for transversions (A↔C, A↔T, G↔C, G↔T - different purine/pyrimidine groups)

3. **Count_Transversions Function**:
   - Takes two DNA strings of equal length
   - Compares each position in the strings
   - Counts how many positions have transversions
   - Returns the total count

4. **Main Procedure**:
   - Reads two DNA strings from standard input
   - Calls the counting function
   - Outputs the result

## Example Usage

If the input is:
```
ACGTACGT
TGCATGCA
```

The comparison would be:
- A vs T (transversion) ✓
- C vs G (transversion) ✓  
- G vs A (transversion) ✓
- T vs T (transition) ✗
- A vs G (transition) ✗
- C vs C (transition) ✗
- G vs A (transversion) ✓
- T vs C (transversion) ✓

Result: 4 transversions

The solution correctly identifies transversions as substitutions between different purine/pyrimidine groups.

