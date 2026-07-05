# Rosalind Problem: Finding a Motif with Modifications (Ada Solution)

## Problem Understanding

In this problem, we need to find all occurrences of a pattern (motif) in a DNA string, including modifications where some positions can match multiple nucleotides.

## Approach

I'll implement a solution that:
1. Reads the DNA string and motif from input
2. Handles wildcard characters that can match multiple nucleotides
3. Finds all starting positions where the motif matches the DNA string

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Finding_A_Motif_With_Modifications is
   
   type Nucleotide is ('A', 'C', 'G', 'T');
   type Motif_Character is record
      Is_Wildcard : Boolean;
      Possible_Nucleotides : array(1..4) of Boolean; -- A, C, G, T
   end record;
   
   type Motif_Array is array(Natural range <>) of Motif_Character;
   
   function Is_Match(Motif : Motif_Character; Nucleotide : Character) return Boolean is
   begin
      if Motif.Is_Wildcard then
         case Nucleotide is
            when 'A' => return Motif.Possible_Nucleotides(1);
            when 'C' => return Motif.Possible_Nucleotides(2);
            when 'G' => return Motif.Possible_Nucleotides(3);
            when 'T' => return Motif.Possible_Nucleotides(4);
            when others => return False;
         end case;
      else
         case Nucleotide is
            when 'A' => return Motif.Possible_Nucleotides(1);
            when 'C' => return Motif.Possible_Nucleotides(2);
            when 'G' => return Motif.Possible_Nucleotides(3);
            when 'T' => return Motif.Possible_Nucleotides(4);
            when others => return False;
         end case;
      end if;
   end Is_Match;
   
   function Build_Motif(Motif_String : String) return Motif_Array is
      Result : Motif_Array(1..Motif_String'Length);
      Position : Natural := 1;
   begin
      for I in Motif_String'Range loop
         case Motif_String(I) is
            when 'A' =>
               Result(Position).Is_Wildcard := False;
               Result(Position).Possible_Nucleotides := (True, False, False, False);
            when 'C' =>
               Result(Position).Is_Wildcard := False;
               Result(Position).Possible_Nucleotides := (False, True, False, False);
            when 'G' =>
               Result(Position).Is_Wildcard := False;
               Result(Position).Possible_Nucleotides := (False, False, True, False);
            when 'T' =>
               Result(Position).Is_Wildcard := False;
               Result(Position).Possible_Nucleotides := (False, False, False, True);
            when 'N' | 'R' | 'Y' | 'M' | 'K' | 'S' | 'W' | 'B' | 'D' | 'H' | 'V' =>
               -- Handle IUPAC ambiguity codes
               Result(Position).Is_Wildcard := True;
               case Motif_String(I) is
                  when 'N' => -- Any nucleotide
                     Result(Position).Possible_Nucleotides := (True, True, True, True);
                  when 'R' => -- Purine (A or G)
                     Result(Position).Possible_Nucleotides := (True, False, True, False);
                  when 'Y' => -- Pyrimidine (C or T)
                     Result(Position).Possible_Nucleotides := (False, True, False, True);
                  when 'M' => -- A or C
                     Result(Position).Possible_Nucleotides := (True, True, False, False);
                  when 'K' => -- G or T
                     Result(Position).Possible_Nucleotides := (False, False, True, True);
                  when 'S' => -- G or C
                     Result(Position).Possible_Nucleotides := (False, True, True, False);
                  when 'W' => -- A or T
                     Result(Position).Possible_Nucleotides := (True, False, False, True);
                  when 'B' => -- C, G, or T
                     Result(Position).Possible_Nucleotides := (False, True, True, True);
                  when 'D' => -- A, G, or T
                     Result(Position).Possible_Nucleotides := (True, False, True, True);
                  when 'H' => -- A, C, or T
                     Result(Position).Possible_Nucleotides := (True, True, False, True);
                  when 'V' => -- A, C, or G
                     Result(Position).Possible_Nucleotides := (True, True, True, False);
                  when others =>
                     Result(Position).Possible_Nucleotides := (False, False, False, False);
               end case;
            when others =>
               Result(Position).Is_Wildcard := False;
               Result(Position).Possible_Nucleotides := (False, False, False, False);
         end case;
         Position := Position + 1;
      end loop;
      return Result;
   end Build_Motif;
   
   function Matches(Motif : Motif_Array; DNA : String; Start_Pos : Natural) return Boolean is
      Length : constant Natural := Motif'Length;
   begin
      if Start_Pos + Length - 1 > DNA'Length then
         return False;
      end if;
      
      for I in Motif'Range loop
         if not Is_Match(Motif(I), DNA(Start_Pos + I - 1)) then
            return False;
         end if;
      end loop;
      
      return True;
   end Matches;
   
   function Find_All_Matches(DNA : String; Motif : Motif_Array) return String is
      Positions : Unbounded_String;
      Found_First : Boolean := False;
   begin
      for I in 1..(DNA'Length - Motif'Length + 1) loop
         if Matches(Motif, DNA, I) then
            if Found_First then
               Append(Positions, " ");
            end if;
            Append(Positions, Integer'Image(I));
            Found_First := True;
         end if;
      end loop;
      
      return To_String(Positions);
   end Find_All_Matches;
   
   -- Read input
   DNA_String : Unbounded_String;
   Motif_String : Unbounded_String;
   
begin
   -- Read DNA string and motif
   Get_Line(DNA_String);
   Get_Line(Motif_String);
   
   -- Build motif structure
   declare
      Motif : constant Motif_Array := Build_Motif(To_String(Motif_String));
   begin
      -- Find all matches and output positions (1-indexed)
      Put_Line(Find_All_Matches(To_String(DNA_String), Motif));
   end;
   
end Finding_A_Motif_With_Modifications;
```

## Explanation

This Ada solution handles the motif finding problem with modifications by:

1. **Data Structures**: 
   - `Motif_Character` record to represent each character in the motif, including whether it's a wildcard and what nucleotides it can match
   - `Motif_Array` to store all characters of the motif

2. **Wildcard Handling**:
   - Supports IUPAC ambiguity codes like N (any), R (purine), Y (pyrimidine), etc.
   - Converts each wildcard character into possible nucleotide matches

3. **Matching Logic**:
   - `Is_Match` function checks if a DNA nucleotide matches a motif character
   - `Matches` function compares the motif against a substring of the DNA
   - `Find_All_Matches` finds all starting positions where the motif occurs

4. **Output**:
   - Returns 1-indexed positions (as required by Rosalind)
   - Space-separated positions for multiple matches

## Example Usage

For input:
```
ACGTACGTACGT
ACG
```

The output would be:
```
1 5 9
```

This solution handles standard nucleotides and common IUPAC ambiguity codes, making it suitable for the Rosalind problem requirements.