# Rosalind Problem: Creating a Restriction Map

This problem asks us to find the positions of restriction sites on a DNA sequence given the lengths of fragments produced by digestion.

## Problem Understanding

Given:
- A DNA sequence
- A list of fragment lengths that result from cutting the sequence with a restriction enzyme

We need to determine all possible positions where the restriction sites could be located.

## Solution Approach

The key insight is that if we have fragment lengths, we can calculate the cumulative distances and use those to identify potential restriction site positions.

Let me implement this step by step:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Creating_A_Restriction_Map is
   
   type Fragment_Lengths is array (Positive range <>) of Positive;
   
   -- Function to compute all possible restriction maps
   function Compute_Restriction_Maps(Sequence : Unbounded_String;
                                    Fragments : Fragment_Lengths) 
                                    return Fragment_Lengths is
      Length : constant Positive := Length(Sequence);
      Total_Length : Positive := 0;
      Map : Fragment_Lengths(1..Fragments'Length);
      
   begin
      -- Calculate total length of fragments
      for I in Fragments'Range loop
         Total_Length := Total_Length + Fragments(I);
      end loop;
      
      -- Check if fragments match sequence length
      if Total_Length /= Length then
         raise Program_Error with "Fragment lengths don't match sequence length";
      end if;
      
      -- Build the map - positions are cumulative sums of fragment lengths
      Map(1) := Fragments(1);
      for I in 2..Fragments'Length loop
         Map(I) := Map(I-1) + Fragments(I);
      end loop;
      
      return Map;
   end Compute_Restriction_Maps;
   
   -- Function to check if a given set of positions is valid
   function Is_Valid_Map(Sequence : Unbounded_String;
                        Positions : Fragment_Lengths;
                        Expected_Fragments : Fragment_Lengths) 
                        return Boolean is
      Length : constant Positive := Length(Sequence);
      Current_Position : Positive := 1;
      Fragment_Count : Positive := 1;
      Current_Length : Positive := 0;
      
   begin
      for I in Positions'Range loop
         -- Check if we're at a restriction site
         if I = 1 then
            -- First position is always the start
            Current_Position := 1;
         else
            -- Calculate fragment length from previous to current position
            Current_Length := Positions(I) - Positions(I-1);
            if Current_Length /= Expected_Fragments(Fragment_Count) then
               return False;
            end if;
            Fragment_Count := Fragment_Count + 1;
         end if;
      end loop;
      
      -- Check last fragment
      if Fragment_Count <= Expected_Fragments'Length then
         Current_Length := Length - Positions(Positions'Last);
         if Current_Length /= Expected_Fragments(Fragment_Count) then
            return False;
         end if;
      end if;
      
      return True;
   end Is_Valid_Map;
   
   -- Main function to solve the problem
   procedure Solve_Restriction_Map(Sequence : Unbounded_String;
                                  Fragments : Fragment_Lengths) is
      Length : constant Positive := Length(Sequence);
      Map : Fragment_Lengths(1..Fragments'Length);
      Valid_Positions : Fragment_Lengths(1..Fragments'Length);
      
   begin
      -- Calculate cumulative positions for fragments
      Map(1) := Fragments(1);
      for I in 2..Fragments'Length loop
         Map(I) := Map(I-1) + Fragments(I);
      end loop;
      
      -- Print the map
      Put("Restriction map positions: ");
      for I in Map'Range loop
         Put(Map(I), Width => 4);
         if I < Map'Last then
            Put(" ");
         end if;
      end loop;
      New_Line;
      
   end Solve_Restriction_Map;
   
   -- Example usage
   Sequence : Unbounded_String := To_Unbounded_String("AACCGGTT");
   Fragments : Fragment_Lengths(1..4) := (2, 3, 2, 3);
   
begin
   Put_Line("DNA Sequence: " & To_String(Sequence));
   Put_Line("Fragment lengths: ");
   for I in Fragments'Range loop
      Put(Fragments(I), Width => 4);
   end loop;
   New_Line;
   
   Solve_Restriction_Map(Sequence, Fragments);
   
end Creating_A_Restriction_Map;
```

## Alternative Implementation (More Direct)

Here's a cleaner implementation that focuses on the core problem:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Creating_A_Restriction_Map is
   
   -- Calculate restriction map from fragment lengths
   procedure Calculate_Map(Fragment_Lengths : in out Integer_Array) is
      Total : Integer := 0;
      Position : Integer := 0;
   begin
      Put("Restriction sites at positions: ");
      
      for I in Fragment_Lengths'Range loop
         Position := Position + Fragment_Lengths(I);
         Put(Position, Width => 4);
         if I < Fragment_Lengths'Last then
            Put(" ");
         end if;
      end loop;
      
      New_Line;
   end Calculate_Map;
   
   -- Sample test data
   Test_Fragments : constant array (1..5) of Integer := 
      (2, 3, 2, 3, 4);
   
begin
   Put_Line("Creating Restriction Map");
   Put_Line("Fragment lengths: ");
   for I in Test_Fragments'Range loop
      Put(Test_Fragments(I), Width => 4);
   end loop;
   New_Line;
   
   Calculate_Map(Test_Fragments);
   
end Creating_A_Restriction_Map;
```

## Key Points

1. **Input**: DNA sequence and fragment lengths from restriction digestion
2. **Output**: Positions where restriction sites occur
3. **Logic**: 
   - Each fragment length represents the distance between consecutive restriction sites
   - Cumulative sum gives us the positions of restriction sites
4. **Example**: With fragments [2, 3, 2, 3], if sequence is 10 bases long:
   - First site: position 2 (2 bases)
   - Second site: position 5 (2+3=5)
   - Third site: position 7 (5+2=7)
   - Fourth site: position 10 (7+3=10)

The solution calculates cumulative positions of restriction sites based on the given fragment lengths.