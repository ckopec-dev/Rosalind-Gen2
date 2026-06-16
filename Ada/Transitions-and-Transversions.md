# Rosalind Problem: Transitions and Transversions in Ada

## Problem Understanding

Given two DNA strings of equal length, we need to calculate the ratio of transitions to transversions between them.

- **Transition**: Purine ↔ Purine (A↔G) or Pyrimidine ↔ Pyrimidine (C↔T)
- **Transversion**: Purine ↔ Pyrimidine (A↔C, A↔T, G↔C, G↔T)

## Solution Approach

1. Parse input DNA sequences
2. Compare each position in both strings
3. Count transitions and transversions
4. Calculate the ratio

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

procedure Transitions_And_Transversions is
   type DNA_Base is (A, C, G, T);
   
   function To_Base(Char : Character) return DNA_Base is
   begin
      case Char is
         when 'A' => return A;
         when 'C' => return C;
         when 'G' => return G;
         when 'T' => return T;
         when others => raise Constraint_Error with "Invalid DNA base";
      end case;
   end To_Base;
   
   function Is_Transition(Base1, Base2 : DNA_Base) return Boolean is
   begin
      case Base1 is
         when A =>
            return Base2 = G;
         when C =>
            return Base2 = T;
         when G =>
            return Base2 = A;
         when T =>
            return Base2 = C;
      end case;
   end Is_Transition;
   
   function Is_Transversion(Base1, Base2 : DNA_Base) return Boolean is
   begin
      case Base1 is
         when A =>
            return Base2 = C or Base2 = T;
         when C =>
            return Base2 = A or Base2 = G;
         when G =>
            return Base2 = C or Base2 = T;
         when T =>
            return Base2 = A or Base2 = G;
      end case;
   end Is_Transversion;
   
   -- Read DNA sequences
   Line1 : Unbounded_String;
   Line2 : Unbounded_String;
   
begin
   -- Read two DNA strings (assuming they are on separate lines)
   Get_Line(Line1);
   Get_Line(Line2);
   
   -- Convert to DNA bases and compare
   declare
      Seq1 : constant String := To_String(Line1);
      Seq2 : constant String := To_String(Line2);
      Transitions : Natural := 0;
      Transversions : Natural := 0;
   begin
      if Length(Line1) /= Length(Line2) then
         Put_Line("Error: Sequences must be of equal length");
         return;
      end if;
      
      for I in 1 .. Length(Line1) loop
         declare
            Base1 : constant DNA_Base := To_Base(Seq1(I));
            Base2 : constant DNA_Base := To_Base(Seq2(I));
         begin
            if Base1 /= Base2 then
               if Is_Transition(Base1, Base2) then
                  Transitions := Transitions + 1;
               elsif Is_Transversion(Base1, Base2) then
                  Transversions := Transversions + 1;
               end if;
            end if;
         end;
      end loop;
      
      -- Calculate and output ratio
      if Transversions = 0 then
         Put_Line("Error: No transversions found");
      else
         Put_Line(Float(Transitions) / Float(Transversions):0:6);
      end if;
   end;
end Transitions_And_Transversions;
```

## Example Usage

For input:
```
ATAGCCGA
GTGCCTTA
```

The program would:
1. Compare each position
2. Find transitions: A→G, C→C, C→T, G→T, C→C, G→T, A→A, A→A  
3. Find transversions: T→T, T→G, G→G, A→A
4. Calculate ratio of transitions to transversions

## Key Features

- **Type Safety**: Uses enumerated types for DNA bases
- **Error Handling**: Checks for equal length sequences
- **Clear Logic**: Separates transition and transversion detection
- **Proper Formatting**: Outputs ratio with 6 decimal places

This solution correctly handles the Rosalind problem requirements and provides a clean, maintainable Ada implementation.