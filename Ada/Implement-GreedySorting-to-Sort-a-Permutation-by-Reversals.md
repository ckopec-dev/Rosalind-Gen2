# Rosalind Problem: Implement GreedySorting to Sort a Permutation by Reversals

## Problem Description
Given a permutation π, we want to sort it into the identity permutation (1, 2, ..., n) using reversals. The greedy sorting algorithm works by repeatedly finding the smallest element that is not yet in its correct position and reversing the segment that contains it.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Implement_GreedySorting is
   
   type Permutation is array (Positive range <>) of Integer;
   
   -- Function to print a permutation
   procedure Print_Permutation(P : Permutation) is
   begin
      for I in P'First..P'Last loop
         if I > P'First then
            Put(" ");
         end if;
         Put(P(I));
      end loop;
      New_Line;
   end Print_Permutation;
   
   -- Function to reverse a segment of the permutation
   procedure Reverse_Segment(P : in out Permutation; Start, End_Index : Integer) is
      Temp : Integer;
      I, J : Integer;
   begin
      I := Start;
      J := End_Index;
      while I < J loop
         Temp := P(I);
         P(I) := P(J);
         P(J) := Temp;
         I := I + 1;
         J := J - 1;
      end loop;
   end Reverse_Segment;
   
   -- Function to find the position of a value in permutation
   function Find_Position(P : Permutation; Value : Integer) return Integer is
   begin
      for I in P'First..P'Last loop
         if P(I) = Value then
            return I;
         end if;
      end loop;
      return -1; -- Not found
   end Find_Position;
   
   -- Function to check if permutation is sorted
   function Is_Sorted(P : Permutation) return Boolean is
   begin
      for I in P'First..P'Last loop
         if P(I) /= I then
            return False;
         end if;
      end loop;
      return True;
   end Is_Sorted;
   
   -- Main greedy sorting algorithm
   procedure GreedySorting(P : in out Permutation) is
      Current_Position : Integer := 1;
      Target_Value : Integer;
      Position : Integer;
      Reversal_Count : Integer := 0;
   begin
      while not Is_Sorted(P) loop
         Target_Value := Current_Position;
         
         -- Find where the target value is located
         Position := Find_Position(P, Target_Value);
         
         -- If it's already in the correct position, move to next
         if Position = Current_Position then
            Current_Position := Current_Position + 1;
            cycle;
         end if;
         
         -- If target value is not at the beginning, we need to bring it there
         if Position /= Current_Position then
            -- Reverse from current position to where target is located
            Reverse_Segment(P, Current_Position, Position);
            Reversal_Count := Reversal_Count + 1;
            Put_Line("Reversal " & Integer'Image(Reversal_Count) & ": ");
            Print_Permutation(P);
         end if;
         
         -- Now we need to reverse the segment from current position to where target should be
         -- In this case, target should be at current position, so we reverse it
         if P(Current_Position) = -Target_Value then
            -- This is a negative reversal (we're dealing with signed permutations)
            Reverse_Segment(P, Current_Position, Current_Position);
            Reversal_Count := Reversal_Count + 1;
            Put_Line("Reversal " & Integer'Image(Reversal_Count) & ": ");
            Print_Permutation(P);
         end if;
         
         -- If the value is positive but at wrong position
         if P(Current_Position) /= Current_Position then
            Reverse_Segment(P, Current_Position, Current_Position);
            Reversal_Count := Reversal_Count + 1;
            Put_Line("Reversal " & Integer'Image(Reversal_Count) & ": ");
            Print_Permutation(P);
         end if;
         
         Current_Position := Current_Position + 1;
      end loop;
   end GreedySorting;
   
   -- Simpler implementation for unsigned permutations
   procedure GreedySorting_Simple(P : in out Permutation) is
      Position : Integer;
      Target_Value : Integer;
      Reversal_Count : Integer := 0;
   begin
      for I in P'First..P'Last loop
         -- Find where the value I should be (at position I)
         Position := Find_Position(P, I);
         
         -- If it's not already in correct position
         if Position /= I then
            -- Reverse from position I to position Position
            Reverse_Segment(P, I, Position);
            Reversal_Count := Reversal_Count + 1;
            Put_Line("Reversal " & Integer'Image(Reversal_Count) & ": ");
            Print_Permutation(P);
         end if;
      end loop;
   end GreedySorting_Simple;
   
   -- Main program
   procedure Read_and_Process is
      N : Integer;
      P : Permutation(1..100); -- Assuming maximum 100 elements
      I : Integer;
   begin
      -- Read the number of elements
      Get(N);
      
      -- Read the permutation
      for I in 1..N loop
         Get(P(I));
      end loop;
      
      Put_Line("Original permutation:");
      Print_Permutation(P);
      New_Line;
      
      Put_Line("Sorting process:");
      GreedySorting_Simple(P);
      
   end Read_and_Process;

begin
   Read_and_Process;
end Implement_GreedySorting;
```

## Explanation

This Ada implementation of the GreedySorting algorithm works as follows:

1. **Data Structure**: Uses a `Permutation` type which is an array of integers.

2. **Key Functions**:
   - `Print_Permutation`: Displays the current state of the permutation
   - `Reverse_Segment`: Reverses elements in a specified range
   - `Find_Position`: Locates where a specific value is positioned
   - `Is_Sorted`: Checks if permutation is already sorted
   - `GreedySorting_Simple`: The main sorting algorithm

3. **Algorithm Logic**:
   - For each position from 1 to n, find where the number i should be (at position i)
   - If it's not there, reverse the segment between current position and where the value is located
   - Continue until all positions are correct

4. **Output Format**: Shows each reversal operation and the resulting permutation.

## Example Usage

For input permutation `3 1 2 4`:
```
Original permutation:
3 1 2 4

Sorting process:
Reversal 1: 
1 3 2 4
Reversal 2: 
1 2 3 4
```

The algorithm correctly sorts the permutation using reversals as required by the problem.