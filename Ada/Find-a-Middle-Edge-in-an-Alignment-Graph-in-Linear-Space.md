# Rosalind Problem: Find a Middle Edge in an Alignment Graph in Linear Space

## Problem Description
Given two strings, find a middle edge in the alignment graph that can be used for divide-and-conquer approach to sequence alignment.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Find_Middle_Edge is
   
   -- String type definition
   type String_Type is array (Positive range <>) of Character;
   
   -- Function to compute the middle edge in linear space
   function Middle_Edge(text1, text2 : String_Type) return String is
      n : constant Natural := text1'Length;
      m : constant Natural := text2'Length;
      
      -- Compute the score matrix for the alignment
      type Score_Matrix is array (0..n, 0..m) of Integer;
      score : Score_Matrix;
      
      -- Initialize the matrix
      procedure Initialize_Matrix is
      begin
         -- Initialize first row and column to zero
         for i in 0..n loop
            score(i, 0) := 0;
         end loop;
         
         for j in 0..m loop
            score(0, j) := 0;
         end loop;
      end Initialize_Matrix;
      
   begin
      -- Initialize the matrix
      Initialize_Matrix;
      
      -- Fill the matrix using dynamic programming
      for i in 1..n loop
         for j in 1..m loop
            if text1(i) = text2(j) then
               score(i, j) := score(i-1, j-1) + 1;
            else
               score(i, j) := max(score(i-1, j), score(i, j-1));
            end if;
         end loop;
      end loop;
      
      -- Find the middle edge (middle row of the matrix)
      declare
         mid_row : constant Natural := n / 2;
         max_score : Integer := -1;
         max_col : Natural := 0;
         edge : String(1..3);
      begin
         -- Find maximum score in the middle row
         for j in 0..m loop
            if score(mid_row, j) > max_score then
               max_score := score(mid_row, j);
               max_col := j;
            end if;
         end loop;
         
         -- Determine edge type based on where the maximum occurs
         if max_col = 0 then
            edge := "D";  -- Down edge (deletion)
         elsif max_col = m then
            edge := "R";  -- Right edge (insertion)
         else
            edge := "M";  -- Move edge (match/mismatch)
         end if;
         
         return edge;
      end;
   end Middle_Edge;
   
   -- Alternative implementation using linear space approach
   function Middle_Edge_Linear(text1, text2 : String_Type) return String is
      n : constant Natural := text1'Length;
      m : constant Natural := text2'Length;
      
      -- Only store two rows for space efficiency
      type Row is array (0..m) of Integer;
      prev_row : Row;
      curr_row : Row;
      
      mid_col : constant Natural := m / 2;
      
   begin
      -- Initialize first row
      for j in 0..m loop
         prev_row(j) := 0;
      end loop;
      
      -- Compute scores using only two rows
      for i in 1..n loop
         curr_row(0) := 0;
         
         for j in 1..m loop
            if text1(i) = text2(j) then
               curr_row(j) := prev_row(j-1) + 1;
            else
               curr_row(j) := max(prev_row(j), curr_row(j-1));
            end if;
         end loop;
         
         -- Check if we've reached the middle column
         if j = mid_col then
            -- Determine edge type based on current row values
            if curr_row(mid_col) = prev_row(mid_col) then
               return "D";  -- Down (deletion)
            elsif curr_row(mid_col) = curr_row(mid_col-1) then
               return "R";  -- Right (insertion)
            else
               return "M";  -- Move (match/mismatch)
            end if;
         end if;
         
         -- Swap rows for next iteration
         declare
            temp : Row renames prev_row;
         begin
            prev_row := curr_row;
            curr_row := temp;
         end;
      end loop;
      
      return "M";  -- Default case
   end Middle_Edge_Linear;
   
   -- Main program to test the implementation
   procedure Test_Middle_Edge is
      text1 : constant String_Type := (1 => 'A', 2 => 'B', 3 => 'C', 4 => 'D');
      text2 : constant String_Type := (1 => 'X', 2 => 'Y', 3 => 'Z', 4 => 'W');
      
      result : String;
   begin
      Put_Line("Testing middle edge algorithm:");
      Put_Line("String 1: ABCD");
      Put_Line("String 2: XYZW");
      
      -- Test the linear space approach
      result := Middle_Edge_Linear(text1, text2);
      Put_Line("Middle edge type: " & result);
   end Test_Middle_Edge;
   
begin
   Test_Middle_Edge;
end Find_Middle_Edge;
```

## Explanation

This Ada program implements the solution for finding a middle edge in an alignment graph using linear space complexity:

1. **Data Structures**: 
   - Uses arrays to represent strings and score matrices
   - Implements a two-row approach to maintain O(m) space complexity

2. **Algorithm**:
   - Computes the optimal alignment scores using dynamic programming
   - Identifies the middle row of the alignment matrix
   - Determines the type of middle edge (Down, Right, or Move)

3. **Space Complexity**: 
   - Uses only two rows instead of the full matrix
   - Achieves O(n × m) time complexity with O(m) space complexity

4. **Edge Types**:
   - "D": Down edge (deletion)
   - "R": Right edge (insertion)  
   - "M": Move edge (match/mismatch)

The program demonstrates the divide-and-conquer approach for sequence alignment where finding a middle edge allows splitting the problem into subproblems that can be solved recursively.