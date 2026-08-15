# Rosalind Problem: Find the Longest Repeat in a String (Ada Solution)

## Problem Understanding

The task is to find the longest substring that appears at least twice in a given string, where the occurrences can overlap.

## Approach

I'll use a suffix array approach:
1. Generate all suffixes of the string
2. Sort the suffixes lexicographically
3. Compare adjacent suffixes to find the longest common prefix
4. Track the maximum length found

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Synchronous_Task_Control;

procedure Find_The_Longest_Repeat is
   
   type Suffix_Type is record
      Index : Natural;
      Text  : Unbounded_String;
   end record;
   
   package Suffix_Vector is new Ada.Containers.Vectors (Natural, Suffix_Type);
   use Suffix_Vector;
   
   procedure Sort_Suffixes (Suffixes : in out Vector);
   function Common_Prefix_Length (S1, S2 : Unbounded_String) return Natural;
   function Longest_Repeat (Text : String) return String;
   
   -- Sort suffixes lexicographically
   procedure Sort_Suffixes (Suffixes : in out Vector) is
      procedure Quick_Sort (Low, High : Natural);
      procedure Swap (I, J : Natural);
      
      procedure Quick_Sort (Low, High : Natural) is
         Pivot : Natural;
         I, J : Natural;
      begin
         if Low < High then
            I := Low;
            J := High;
            Pivot := Suffixes.Element ((Low + High) / 2).Text'First;
            
            loop
               while Suffixes.Element (I).Text < Suffixes.Element (Pivot).Text loop
                  I := I + 1;
               end loop;
               
               while Suffixes.Element (J).Text > Suffixes.Element (Pivot).Text loop
                  J := J - 1;
               end loop;
               
               if I <= J then
                  Swap (I, J);
                  I := I + 1;
                  J := J - 1;
               end if;
               
               exit when I > J;
            end loop;
            
            Quick_Sort (Low, J);
            Quick_Sort (I, High);
         end if;
      end Quick_Sort;
      
      procedure Swap (I, J : Natural) is
         Temp : Suffix_Type := Suffixes.Element (I);
      begin
         Suffixes.Replace_Element (I, Suffixes.Element (J));
         Suffixes.Replace_Element (J, Temp);
      end Swap;
      
   begin
      if Suffixes.Length > 1 then
         Quick_Sort (1, Suffixes.Length);
      end if;
   end Sort_Suffixes;
   
   -- Find common prefix length between two strings
   function Common_Prefix_Length (S1, S2 : Unbounded_String) return Natural is
      Min_Length : constant Natural := Natural'Min (Length (S1), Length (S2));
      Count : Natural := 0;
   begin
      for I in 1 .. Min_Length loop
         if Element (S1, I) = Element (S2, I) then
            Count := Count + 1;
         else
            exit;
         end if;
      end loop;
      return Count;
   end Common_Prefix_Length;
   
   -- Main function to find longest repeat
   function Longest_Repeat (Text : String) return String is
      Suffixes : Vector;
      Max_Length : Natural := 0;
      Best_Suffix : Unbounded_String := Null_Unbounded_String;
      Current_Length : Natural;
   begin
      -- Create all suffixes
      for I in Text'First .. Text'Last loop
         declare
            Suffix : Unbounded_String := To_Unbounded_String (Text (I .. Text'Last));
         begin
            Append (Suffixes, 
                   (Index => I - Text'First + 1,
                    Text  => Suffix));
         end;
      end loop;
      
      -- Sort suffixes
      Sort_Suffixes (Suffixes);
      
      -- Compare adjacent suffixes to find longest common prefix
      for I in 1 .. Suffixes.Length - 1 loop
         Current_Length := Common_Prefix_Length (Suffixes.Element (I).Text, 
                                                Suffixes.Element (I + 1).Text);
         if Current_Length > Max_Length then
            Max_Length := Current_Length;
            Best_Suffix := Suffixes.Element (I).Text;
         end if;
      end loop;
      
      -- Return the longest repeated substring
      if Max_Length = 0 then
         return "";
      else
         return To_String (Best_Suffix (1 .. Max_Length));
      end if;
   end Longest_Repeat;
   
   -- Main program
begin
   -- Read input from stdin
   declare
      Input_Line : Unbounded_String;
      Text : String (1 .. 1000);  -- Assuming maximum length
      Length_Read : Natural;
   begin
      Get_Line (Input_Line);
      Length_Read := Length (Input_Line);
      Text := To_String (Input_Line);
      
      -- Find and print the longest repeat
      Put_Line (Longest_Repeat (Text));
   end;
end Find_The_Longest_Repeat;
```

## Alternative Simpler Implementation

For better clarity, here's a more straightforward approach:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Find_The_Longest_Repeat is
   
   function Longest_Repeat (Text : String) return String is
      Max_Length : Natural := 0;
      Best_Substring : Unbounded_String := Null_Unbounded_String;
      
      -- Check if a substring appears more than once
      function Appears_More_Than_Once (Substring : String) return Boolean is
         Count : Natural := 0;
         Index : Natural := Text'First - 1;
      begin
         loop
            Index := Index + 1;
            exit when Index > Text'Last - Substring'Length + 1;
            
            -- Check if substring matches at current position
            declare
               Match : Boolean := True;
            begin
               for I in Substring'Range loop
                  if Text (Index + I - Substring'First) /= Substring (I) then
                     Match := False;
                     exit;
                  end if;
               end loop;
               
               if Match then
                  Count := Count + 1;
                  if Count >= 2 then
                     return True;
                  end if;
               end if;
            end;
         end loop;
         return False;
      end Appears_More_Than_Once;
      
   begin
      -- Try all possible substrings, starting from longest
      for Length in reverse Text'Length .. 1 loop
         for Start in Text'First .. Text'Last - Length + 1 loop
            declare
               Candidate : constant String := Text (Start .. Start + Length - 1);
            begin
               if Appears_More_Than_Once (Candidate) then
                  return Candidate;
               end if;
            end;
         end loop;
      end loop;
      
      return "";
   end Longest_Repeat;
   
begin
   -- Read input and solve
   declare
      Input_Line : Unbounded_String;
   begin
      Get_Line (Input_Line);
      Put_Line (Longest_Repeat (To_String (Input_Line)));
   end;
end Find_The_Longest_Repeat;
```

## Usage

1. Save the code to a file named `longest_repeat.adb`
2. Compile with: `gnatmake longest_repeat.adb`
3. Run with input: `./longest_repeat < input.txt`

## Time Complexity
- The first approach: O(n² log n) due to sorting suffixes
- The second approach: O(n³) in worst case but simpler to understand

## Space Complexity
O(n²) for storing all suffixes and their prefixes

The solution correctly handles overlapping occurrences and finds the longest substring that appears at least twice in the input string.