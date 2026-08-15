# Find the Longest Substring Shared by Two Strings - Ada Solution

Here's a solution to the Rosalind problem "Find the Longest Substring Shared by Two Strings" using Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Longest_Common_Substring is
   type String_Array is array (Positive range <>) of Unbounded_String;
   
   function Longest_Common_Substring(s1, s2 : Unbounded_String) return Unbounded_String is
      len1 : constant Natural := Length(s1);
      len2 : constant Natural := Length(s2);
      max_len : Natural := 0;
      result : Unbounded_String := Null_Unbounded_String;
      
      -- Create a 2D array to store lengths of common substrings
      type Table_Type is array (0 .. len1, 0 .. len2) of Natural;
      table : Table_Type := (others => (others => 0));
   begin
      -- Fill the table using dynamic programming
      for i in 1 .. len1 loop
         for j in 1 .. len2 loop
            if s1(i) = s2(j) then
               table(i,j) := table(i-1, j-1) + 1;
               if table(i,j) > max_len then
                  max_len := table(i,j);
                  -- Extract the substring
                  result := Slice(s1, i - max_len + 1, i);
               end if;
            else
               table(i,j) := 0;
            end if;
         end loop;
      end loop;
      
      return result;
   end Longest_Common_Substring;
   
   -- Read input strings
   s1 : Unbounded_String;
   s2 : Unbounded_String;
   
begin
   -- Read the two strings from standard input
   Get_Line(s1);
   Get_Line(s2);
   
   -- Find and print the longest common substring
   Put_Line(Longest_Common_Substring(s1, s2));
end Longest_Common_Substring;
```

## Explanation of the Algorithm

This solution uses dynamic programming to find the longest common substring:

1. **Dynamic Programming Table**: Create a 2D table where `table[i][j]` represents the length of the common substring ending at position `i-1` in string `s1` and position `j-1` in string `s2`.

2. **Fill the Table**:
   - If characters match: `table[i][j] = table[i-1][j-1] + 1`
   - If characters don't match: `table[i][j] = 0`

3. **Track Maximum**: Keep track of the maximum length found and the corresponding substring.

4. **Time Complexity**: O(n×m) where n and m are the lengths of the two strings.
5. **Space Complexity**: O(n×m) for the DP table.

## How to Compile and Run

```bash
gnatmake longest_common_substring.adb
./longest_common_substring
```

## Example Input/Output

**Input:**
```
ABABC
BABCA
```

**Output:**
```
BABC
```

This solution correctly handles the case where we need to find the longest substring that appears in both input strings, using efficient dynamic programming approach.