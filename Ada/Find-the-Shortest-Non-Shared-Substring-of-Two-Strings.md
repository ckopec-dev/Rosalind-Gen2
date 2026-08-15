# Finding the Shortest Non-Shared Substring of Two Strings in Ada

I'll solve this step by step, implementing a solution for finding the shortest substring that appears in one string but not in another.

## Problem Understanding

We need to find the shortest substring that:
1. Appears in the first string
2. Does NOT appear in the second string

This is essentially finding the shortest "unique" substring of the first string that's not present in the second string.

## Solution Approach

I'll use a brute force approach:
1. Generate all substrings of the first string, ordered by length
2. For each substring, check if it exists in the second string
3. Return the first substring that doesn't exist in the second string

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Find_The_Shortest_Non_Shared_Substring is
   
   type String_Access is access all String;
   
   function Contains_Substring(Whole : String; Part : String) return Boolean is
   begin
      if Part'Length = 0 then
         return True;
      end if;
      
      for I in Whole'First .. Whole'Last - Part'Length + 1 loop
         if Whole(I .. I + Part'Length - 1) = Part then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Substring;
   
   function Get_All_Substrings(S : String) return String_Array is
      Substrings : array(1 .. (S'Length * (S'Length + 1)) / 2) of Unbounded_String;
      Count : Integer := 0;
   begin
      for I in S'First .. S'Last loop
         for J in I .. S'Last loop
            Count := Count + 1;
            Substrings(Count) := To_Unbounded_String(S(I .. J));
         end loop;
      end loop;
      
      -- Sort by length (shortest first)
      declare
         Result : String_Array(1 .. Count);
      begin
         for I in 1 .. Count loop
            Result(I) := To_String(Substrings(I));
         end loop;
         return Result;
      end;
   end Get_All_Substrings;
   
   function Find_Shortest_Non_Shared_Substring(S1, S2 : String) return String is
      Substrings : array(1 .. (S1'Length * (S1'Length + 1)) / 2) of Unbounded_String;
      Count : Integer := 0;
      Current_Length : Integer := 1;
      Found : Boolean := False;
      Result : Unbounded_String;
   begin
      -- Generate all substrings of S1, ordered by length
      for I in S1'First .. S1'Last loop
         for J in I .. S1'Last loop
            Count := Count + 1;
            Substrings(Count) := To_Unbounded_String(S1(I .. J));
         end loop;
      end loop;
      
      -- Check substrings from shortest to longest
      for Length in 1 .. S1'Length loop
         for I in 1 .. Count loop
            if Substrings(I)'Length = Length then
               if not Contains_Substring(S2, To_String(Substrings(I))) then
                  return To_String(Substrings(I));
               end if;
            end if;
         end loop;
      end loop;
      
      -- Should never reach here if inputs are valid
      return "";
   end Find_Shortest_Non_Shared_Substring;

   -- Main test function
   procedure Test_Function is
      S1 : constant String := "ABCD";
      S2 : constant String := "BCD";
      Result : String;
   begin
      Put_Line("String 1: " & S1);
      Put_Line("String 2: " & S2);
      
      Result := Find_Shortest_Non_Shared_Substring(S1, S2);
      
      if Result = "" then
         Put_Line("No non-shared substring found");
      else
         Put_Line("Shortest non-shared substring: """ & Result & """");
         Put_Line("Length: " & Integer'image(Result'Length));
      end if;
   end Test_Function;

begin
   Test_Function;
end Find_The_Shortest_Non_Shared_Substring;
```

## Optimized Version

Here's a more efficient version that sorts substrings by length during generation:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Find_The_Shortest_Non_Shared_Substring is
   
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   
   function Contains_Substring(Whole : String; Part : String) return Boolean is
   begin
      if Part'Length = 0 then
         return True;
      end if;
      
      for I in Whole'First .. Whole'Last - Part'Length + 1 loop
         if Whole(I .. I + Part'Length - 1) = Part then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Substring;
   
   function Find_Shortest_Non_Shared_Substring(S1, S2 : String) return String is
      Substrings_By_Length : array(0 .. S1'Length) of String_Vectors.Vector;
      Current_Length : Integer := 1;
   begin
      -- Generate all substrings and place them by length
      for I in S1'First .. S1'Last loop
         for J in I .. S1'Last loop
            declare
               Substring : constant String := S1(I .. J);
               Length : constant Integer := Substring'Length;
            begin
               String_Vectors.Append(Substrings_By_Length(Length), To_Unbounded_String(Substring));
            end;
         end loop;
      end loop;
      
      -- Check substrings from shortest to longest
      for Length in 1 .. S1'Length loop
         if not String_Vectors.Is_Empty(Substrings_By_Length(Length)) then
            for I in 1 .. String_Vectors.Length(Substrings_By_Length(Length)) loop
               declare
                  Current_Substring : constant String := To_String(String_Vectors.Element(Substrings_By_Length(Length), I));
               begin
                  if not Contains_Substring(S2, Current_Substring) then
                     return Current_Substring;
                  end if;
               end;
            end loop;
         end if;
      end loop;
      
      -- Should never reach here if inputs are valid
      return "";
   end Find_Shortest_Non_Shared_Substring;

begin
   Put_Line("Testing with example strings:");
   Put_Line("String 1: ABCD");
   Put_Line("String 2: BCD");
   
   declare
      Result : constant String := Find_Shortest_Non_Shared_Substring("ABCD", "BCD");
   begin
      if Result /= "" then
         Put_Line("Shortest non-shared substring: """ & Result & """");
      else
         Put_Line("No non-shared substring found");
      end if;
   end;
   
end Find_The_Shortest_Non_Shared_Substring;
```

## Explanation

1. **Contains_Substring Function**: Checks if a substring exists within a string using brute force comparison.

2. **Main Algorithm**:
   - Generate all substrings of the first string
   - Group them by length to ensure we check shorter substrings first
   - For each length, check all substrings from shortest to longest
   - Return the first substring that doesn't exist in the second string

3. **Time Complexity**: O(n³) where n is the length of the strings, since we generate up to n² substrings and for each we do up to n comparisons.

4. **Space Complexity**: O(n²) for storing all substrings.

## Example Output
For input strings "ABCD" and "BCD":
- All substrings of "ABCD": A, B, C, D, AB, BC, CD, ABC, BCD, CDE, ...
- Substrings that exist in "BCD": B, C, D, BC, CD, BCD
- Shortest substring not in "BCD": "A"

This solution efficiently finds the shortest non-shared substring by systematically checking substrings from shortest to longest.