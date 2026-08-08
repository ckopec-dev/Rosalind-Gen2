# Rosalind Problem: Compute the Number of Breakpoints in a Permutation (Ada Solution)

## Problem Understanding

A breakpoint in a permutation is a position where two consecutive elements are not consecutive in value. For example, in the permutation [1, 3, 2, 4], there are breakpoints at positions 1 and 3 because:
- Between 1 and 3: 1 and 3 are not consecutive
- Between 3 and 2: 3 and 2 are not consecutive  
- Between 2 and 4: 2 and 4 are not consecutive

We need to count all such breakpoints in a given permutation.

## Solution Approach

1. Add sentinel values (0 at start, n+1 at end) to handle edge cases
2. Iterate through adjacent pairs of elements
3. Count positions where consecutive elements differ by more than 1
4. The number of breakpoints equals the count of such positions

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Compute_Breakpoints is
   type Permutation is array (Positive range <>) of Integer;
   
   function Count_Breakpoints(P : Permutation) return Integer is
      N : constant Integer := P'Length;
      Count : Integer := 0;
      
      -- Create extended permutation with sentinels
      Extended : array (0 .. N + 1) of Integer;
   begin
      -- Initialize extended array with sentinels
      Extended(0) := 0;
      for I in 1 .. N loop
         Extended(I) := P(I);
      end loop;
      Extended(N + 1) := N + 1;
      
      -- Count breakpoints
      for I in 0 .. N loop
         if Extended(I + 1) - Extended(I) /= 1 then
            Count := Count + 1;
         end if;
      end loop;
      
      return Count;
   end Count_Breakpoints;
   
   -- Read permutation from input
   function Read_Permutation return Permutation is
      N : Integer;
      P : Permutation(1 .. 100);  -- Assuming maximum 100 elements
      I : Integer := 1;
      Value : Integer;
   begin
      -- First read the number of elements (we'll read until end of line)
      loop
         if not Ada.Text_IO.End_Of_Line then
            Get(Value);
            P(I) := Value;
            I := I + 1;
         else
            exit;
         end if;
      end loop;
      
      -- Return only the actual elements
      return P(1 .. I - 1);
   end Read_Permutation;
   
begin
   -- Example usage with the sample permutation
   -- For the problem, we would read from input but for demonstration:
   declare
      Sample : Permutation(1 .. 4) := (1, 3, 2, 4);
      Result : Integer;
   begin
      Result := Count_Breakpoints(Sample);
      Put_Line("Number of breakpoints: " & Integer'Image(Result));
   end;
   
   -- For reading from stdin:
   -- declare
   --    Input_Permutation : Permutation(1 .. 100);
   --    Num_Elements : Integer;
   -- begin
   --    Input_Permutation := Read_Permutation;
   --    Put_Line("Breakpoints: " & Integer'Image(Count_Breakpoints(Input_Permutation)));
   -- end;
   
end Compute_Breakpoints;
```

## Alternative Simpler Version

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Breakpoint_Count is
   type Int_Array is array (Positive range <>) of Integer;
   
   function Count_Breaks(P : Int_Array) return Integer is
      Count : Integer := 0;
      
      -- Add sentinels: 0 at beginning, length+1 at end
      function Get_Value(Index : Integer) return Integer is
      begin
         if Index = 0 then
            return 0;
         elsif Index = P'Length + 1 then
            return P'Length + 1;
         else
            return P(Index);
         end if;
      end Get_Value;
      
   begin
      for I in 0 .. P'Length loop
         if Get_Value(I + 1) - Get_Value(I) /= 1 then
            Count := Count + 1;
         end if;
      end loop;
      
      return Count;
   end Count_Breaks;
   
begin
   -- Example: [1, 3, 2, 4]
   declare
      Test : Int_Array(1 .. 4) := (1, 3, 2, 4);
      Result : Integer;
   begin
      Result := Count_Breaks(Test);
      Put_Line("Breakpoints: " & Integer'Image(Result));
   end;
   
end Breakpoint_Count;
```

## Explanation

The algorithm works by:

1. **Adding sentinels**: We add 0 at the beginning and (n+1) at the end of the permutation to handle edge cases properly
2. **Comparing adjacent elements**: For each pair of consecutive elements in the extended array, we check if their difference is exactly 1
3. **Counting breakpoints**: If the difference is not 1, we increment our breakpoint counter

## Example Walkthrough

For permutation [1, 3, 2, 4]:
- Extended: [0, 1, 3, 2, 4, 5]
- Differences: 1-0=1, 3-1=2, 2-3=-1, 4-2=2, 5-4=1
- Breakpoints where difference ≠ 1: positions 2 and 4 (values 3-1=2 and 2-3=-1)
- Count = 2

The time complexity is O(n) and space complexity is O(1).