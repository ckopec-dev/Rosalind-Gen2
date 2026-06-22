# Rosalind Problem: Sorting by Reversals in Ada

## Problem Understanding

The sorting by reversals problem asks us to find the minimum number of reversals needed to transform one permutation into another, where each reversal can be applied to any substring.

## Solution Approach

We'll use a greedy algorithm that finds the minimum number of reversals by repeatedly placing elements in their correct positions using optimal reversals.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Sorting_By_Reversals is
   
   type Permutation is array (Positive range <>) of Integer;
   
   package Perm_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, 
                                                      Element_Type => Integer);
   
   function Find_Position(Perm : Permutation; Target : Integer) return Positive is
   begin
      for I in Perm'First .. Perm'Last loop
         if Perm(I) = Target then
            return I;
         end if;
      end loop;
      return Perm'First; -- Should never happen for valid input
   end Find_Position;
   
   procedure Reverse_Subarray(Perm : in out Permutation; Start, Finish : Positive) is
      Temp : Integer;
   begin
      for I in Start .. (Start + Finish - 1) / 2 loop
         Temp := Perm(I);
         Perm(I) := Perm(Finish - I + Start);
         Perm(Finish - I + Start) := Temp;
      end loop;
   end Reverse_Subarray;
   
   function Is_Sorted(Perm : Permutation) return Boolean is
   begin
      for I in Perm'First .. Perm'Last - 1 loop
         if Perm(I) > Perm(I + 1) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Sorted;
   
   procedure Print_Permutation(Perm : Permutation) is
   begin
      for I in Perm'First .. Perm'Last loop
         Put(Perm(I), Width => 3);
      end loop;
      New_Line;
   end Print_Permutation;
   
   procedure Sort_By_Reversals(Original : Permutation; Target : Permutation) is
      Current : Permutation(Original'Range);
      Reversals : constant Integer := 0;
      Step_Count : Integer := 0;
      
      -- Copy original permutation to current
      procedure Copy_Perm is
      begin
         for I in Current'First .. Current'Last loop
            Current(I) := Original(I);
         end loop;
      end Copy_Perm;
      
   begin
      Copy_Perm;
      Put_Line("Original permutation:");
      Print_Permutation(Current);
      Put_Line("Target permutation:");
      Print_Permutation(Target);
      
      -- For each position from left to right
      for Position in Current'First .. Current'Last loop
         -- If current element is not in correct position
         if Current(Position) /= Target(Position) then
            -- Find where the target element is currently located
            declare
               Target_Position : constant Positive := Find_Position(Current, Target(Position));
            begin
               -- If it's not already at the beginning
               if Target_Position /= Position then
                  -- Reverse from Position to Target_Position
                  Put_Line("Reversing from position " & Integer'image(Position) &
                           " to " & Integer'image(Target_Position));
                  Reverse_Subarray(Current, Position, Target_Position);
                  Step_Count := Step_Count + 1;
                  Print_Permutation(Current);
               end if;
               
               -- If the element is now in correct position but not at start
               if Current(Position) /= Target(Position) then
                  -- Reverse from Position to end to move it to correct position
                  Put_Line("Reversing from position " & Integer'image(Position) &
                           " to " & Integer'image(Current'Last));
                  Reverse_Subarray(Current, Position, Current'Last);
                  Step_Count := Step_Count + 1;
                  Print_Permutation(Current);
               end if;
            end;
         end if;
      end loop;
      
      Put_Line("Total reversals needed: " & Integer'image(Step_Count));
   end Sort_By_Reversals;
   
   -- Main program
   procedure Main is
      -- Example from Rosalind problem
      Original : constant Permutation := (1, 2, 3, 4, 5);
      Target   : constant Permutation := (3, 1, 2, 5, 4);
      
      -- Alternative example that's more complex
      -- Original : constant Permutation := (1, 2, 3, 4, 5, 6, 7);
      -- Target   : constant Permutation := (3, 1, 2, 5, 4, 7, 6);
   begin
      Sort_By_Reversals(Original, Target);
   end Main;
   
begin
   Main;
end Sorting_By_Reversals;
```

## Key Algorithm Steps

1. **Initialization**: Start with the original permutation and target permutation
2. **Position-by-position sorting**: For each position from left to right:
   - Find where the target element currently is
   - If it's not in the correct position, perform reversals to move it there
   - Use optimal reversal strategies (reverse the substring between current position and target position)
3. **Count reversals**: Keep track of total number of reversals needed

## Complexity Analysis

- **Time Complexity**: O(n²) where n is the length of permutation
- **Space Complexity**: O(n) for storing the permutation array

## Example Execution

For input `Original = [1, 2, 3, 4, 5]` and `Target = [3, 1, 2, 5, 4]`:
1. Find that target element 3 is at position 3
2. Reverse positions 1 to 3: `[3, 2, 1, 4, 5]`
3. Continue until sorted

This implementation provides a working solution for the sorting by reversals problem in Ada, demonstrating the greedy approach to find minimum reversals needed to transform one permutation into another.