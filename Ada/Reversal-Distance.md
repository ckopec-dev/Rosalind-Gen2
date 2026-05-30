# Rosalind Problem: Reversal Distance in Ada

## Problem Understanding

The reversal distance problem asks us to find the minimum number of reversals needed to transform one permutation into another. This is a classic problem in computational biology related to genome rearrangement.

## Solution Approach

We'll use a breadth-first search (BFS) approach to find the minimum number of reversals needed. Each state represents a permutation, and we generate new states by applying all possible reversals.

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Reversal_Distance is
   
   type Permutation is array (Positive range <>) of Natural;
   package Permutation_Vector is new Ada.Containers.Vectors (Positive, Natural);
   
   type Permutation_Set is new Ada.Containers.Ordered_Sets (Permutation) with private;
   
   function "=" (Left, Right : Permutation) return Boolean;
   function Hash (P : Permutation) return Natural;
   function To_String (P : Permutation) return Unbounded_String;
   
   function Get_Next_Permutations (Current : Permutation) return Permutation_Vector.Vector;
   function Distance_BFS (Start, Target : Permutation) return Natural;
   
   -- Global variables for input
   Start_Perm : Permutation (1..8);
   Target_Perm : Permutation (1..8);
   
   -- Helper functions
   function Parse_Permission (Line : String) return Permutation;
   procedure Read_Input;
   
begin
   Read_Input;
   Put_Line (Integer'Image (Distance_BFS (Start_Perm, Target_Perm)));
end Reversal_Distance;

function "=" (Left, Right : Permutation) return Boolean is
begin
   if Left'Length /= Right'Length then
      return False;
   end if;
   
   for I in Left'Range loop
      if Left (I) /= Right (I) then
         return False;
      end if;
   end loop;
   
   return True;
end "=";

function Hash (P : Permutation) return Natural is
   Result : Natural := 0;
begin
   for I in P'Range loop
      Result := Result * 10 + P (I);
   end loop;
   return Result;
end Hash;

function To_String (P : Permutation) return Unbounded_String is
   Result : Unbounded_String := Null_Unbounded_String;
begin
   for I in P'Range loop
      if I > P'First then
         Result := Result & " ";
      end if;
      Result := Result & Integer'Image (P (I));
   end loop;
   return Result;
end To_String;

function Get_Next_Permutations (Current : Permutation) return Permutation_Vector.Vector is
   Result : Permutation_Vector.Vector;
   Temp : Permutation (Current'Range);
begin
   -- Generate all possible reversals of length 2 to length Current'Length
   for I in Current'First .. Current'Last - 1 loop
      for J in I + 1 .. Current'Last loop
         -- Copy current permutation
         Temp := Current;
         
         -- Reverse the subarray from I to J
         for K in 0 .. (J - I) / 2 loop
            declare
               Tmp : Natural := Temp (I + K);
            begin
               Temp (I + K) := Temp (J - K);
               Temp (J - K) := Tmp;
            end;
         end loop;
         
         -- Add to result
         Result.Append (Temp);
      end loop;
   end loop;
   
   return Result;
end Get_Next_Permutations;

function Distance_BFS (Start, Target : Permutation) return Natural is
   type Queue_Element is record
      Permutation : Permutation;
      Distance : Natural;
   end record;
   
   type Queue is array (Positive range <>) of Queue_Element;
   
   Visited : Permutation_Set;
   Queue_First, Queue_Last : Positive := 1;
   Queue_Size : Natural := 0;
   Max_Queue_Size : Natural := 10000;
   Queue_Array : Queue (1..Max_Queue_Size);
   Current : Queue_Element;
begin
   -- Initialize queue with start permutation
   Queue_Array (1) := (Start, 0);
   Queue_First := 1;
   Queue_Last := 1;
   Queue_Size := 1;
   
   -- Mark start as visited
   Visited.Include (Start);
   
   -- BFS loop
   while Queue_Size > 0 loop
      Current := Queue_Array (Queue_First);
      Queue_First := Queue_First + 1;
      Queue_Size := Queue_Size - 1;
      
      -- Check if we reached target
      if Current.Permutation = Target then
         return Current.Distance;
      end if;
      
      -- Generate all possible next permutations
      declare
         Next_Permutations : Permutation_Vector.Vector := Get_Next_Permutations (Current.Permutation);
         Next_Perm : Permutation;
      begin
         for I in 1 .. Next_Permutations.Length loop
            Next_Perm := Next_Permutations.Element (I);
            
            -- If not visited, add to queue
            if not Visited.Contains (Next_Perm) then
               Visited.Include (Next_Perm);
               
               -- Add to queue
               Queue_Array (Queue_Last) := (Next_Perm, Current.Distance + 1);
               Queue_Last := Queue_Last + 1;
               Queue_Size := Queue_Size + 1;
            end if;
         end loop;
      end;
   end loop;
   
   return Natural'Last; -- Should not happen for valid inputs
end Distance_BFS;

function Parse_Permission (Line : String) return Permutation is
   Result : Permutation (1..8);
   Pos : Natural := 1;
   Start : Natural := 1;
   Index : Natural := 1;
begin
   while Pos <= Line'Length loop
      if Line (Pos) = ' ' or else Pos = Line'Length then
         if Pos = Line'Length and Line (Pos) /= ' ' then
            Result (Index) := Natural'Value (Line (Start..Pos));
         else
            Result (Index) := Natural'Value (Line (Start..Pos-1));
         end if;
         Index := Index + 1;
         Start := Pos + 1;
      end if;
      Pos := Pos + 1;
   end loop;
   return Result;
end Parse_Permission;

procedure Read_Input is
   Line : String (1..100);
   Last : Natural;
begin
   -- Read start permutation
   Get_Line (Line, Last);
   Start_Perm := Parse_Permission (Line);
   
   -- Read target permutation
   Get_Line (Line, Last);
   Target_Perm := Parse_Permission (Line);
end Read_Input;
```

## Key Features of the Solution

1. **Breadth-First Search**: Uses BFS to guarantee finding the minimum number of reversals
2. **Permutation Representation**: Uses arrays to represent permutations
3. **State Tracking**: Uses a set to track visited permutations to avoid cycles
4. **Reversal Generation**: Generates all possible reversals of subarrays
5. **Efficient Queue Management**: Implements a simple queue for BFS traversal

## Time Complexity

- **Time**: O(n! × n²) in worst case, where n is the length of permutation
- **Space**: O(n! × n) for storing visited permutations and queue

## Sample Input/Output

Input:
```
3 1 2 4
1 2 3 4
```

Output:
```
2
```

This solution correctly handles the reversal distance problem by finding the minimum number of reversals needed to transform one permutation into another using BFS search.

