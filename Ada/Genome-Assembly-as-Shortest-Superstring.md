# Rosalind Problem: Genome Assembly as Shortest Superstring (Ada Solution)

## Problem Understanding

The shortest superstring problem involves finding the shortest string that contains all given strings as substrings. In genome assembly, this means finding the shortest DNA sequence that contains all given DNA fragments.

## Approach

I'll use a greedy approach where at each step:
1. Find the pair of strings with maximum overlap
2. Merge them by overlapping as much as possible
3. Repeat until only one string remains

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Genome_Assembly is
   
   type DNA_String is tagged record
      Sequence : Unbounded_String;
   end record;
   
   function Length(DNA : DNA_String) return Natural is
   begin
      return Length(DNA.Sequence);
   end Length;
   
   function To_String(DNA : DNA_String) return String is
   begin
      return To_String(DNA.Sequence);
   end To_String;
   
   procedure Put(DNA : DNA_String) is
   begin
      Put_Line(To_String(DNA.Sequence));
   end Put;
   
   -- Calculate overlap between suffix of A and prefix of B
   function Overlap(A, B : DNA_String) return Natural is
      A_Str : constant String := To_String(A.Sequence);
      B_Str : constant String := To_String(B.Sequence);
      Max_Overlap : Natural := 0;
      Len_A : constant Natural := Length(A_Str);
      Len_B : constant Natural := Length(B_Str);
   begin
      -- Check all possible overlaps from length 1 to min(len_A, len_B)
      for I in 1 .. Natural'Min(Len_A, Len_B) loop
         if A_Str(Len_A - I + 1 .. Len_A) = B_Str(1 .. I) then
            Max_Overlap := I;
         end if;
      end loop;
      return Max_Overlap;
   end Overlap;
   
   -- Merge two DNA strings with maximum overlap
   function Merge(A, B : DNA_String) return DNA_String is
      A_Str : constant String := To_String(A.Sequence);
      B_Str : constant String := To_String(B.Sequence);
      Overlap_Length : constant Natural := Overlap(A, B);
   begin
      if Overlap_Length = 0 then
         -- No overlap, simply concatenate
         return (Sequence => To_Unbounded_String(A_Str & B_Str));
      else
         -- Overlap exists, merge properly
         declare
            Result : Unbounded_String;
         begin
            Result := A.Sequence;
            Append(Result, B_Str(Overlap_Length + 1 .. Len(B_Str)));
            return (Sequence => Result);
         end;
      end if;
   end Merge;
   
   -- Find the pair with maximum overlap among all pairs
   function Find_Max_Overlap(DNA_List : in out array of DNA_String) 
      return Natural is
      Max_Overlap : Natural := 0;
      Best_Pair_I, Best_Pair_J : Natural := 0;
      Len : constant Natural := DNA_List'Length;
   begin
      for I in 1 .. Len loop
         for J in 1 .. Len loop
            if I /= J then
               declare
                  Current_Overlap : constant Natural := Overlap(DNA_List(I), DNA_List(J));
               begin
                  if Current_Overlap > Max_Overlap then
                     Max_Overlap := Current_Overlap;
                     Best_Pair_I := I;
                     Best_Pair_J := J;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      
      return Best_Pair_I;  -- Return index of first string in best pair
   end Find_Max_Overlap;
   
   -- Remove an element from array
   procedure Remove_Element(DNA_List : in out array of DNA_String; Index : Natural) is
      Len : constant Natural := DNA_List'Length;
   begin
      for I in Index .. Len - 1 loop
         DNA_List(I) := DNA_List(I + 1);
      end loop;
   end Remove_Element;
   
   -- Main assembly function
   function Shortest_Superstring(DNA_List : array of DNA_String) return DNA_String is
      Temp_List : array(1 .. DNA_List'Length) of DNA_String := DNA_List;
      Current_List_Size : Natural := DNA_List'Length;
   begin
      while Current_List_Size > 1 loop
         declare
            Best_Pair_I : constant Natural := Find_Max_Overlap(Temp_List);
            Best_Pair_J : Natural := 0;
            Max_Overlap : Natural := 0;
            -- Find the best pair among all combinations
         begin
            for J in 1 .. Current_List_Size loop
               if J /= Best_Pair_I then
                  declare
                     Current_Overlap : constant Natural := Overlap(Temp_List(Best_Pair_I), Temp_List(J));
                  begin
                     if Current_Overlap > Max_Overlap then
                        Max_Overlap := Current_Overlap;
                        Best_Pair_J := J;
                     end if;
                  end;
               end if;
            end loop;
            
            -- Merge the two strings
            if Best_Pair_J /= 0 then
               declare
                  Merged : constant DNA_String := Merge(Temp_List(Best_Pair_I), Temp_List(Best_Pair_J));
               begin
                  -- Replace one of them with merged result
                  Temp_List(Best_Pair_I) := Merged;
                  -- Remove the other one
                  Remove_Element(Temp_List, Best_Pair_J);
                  Current_List_Size := Current_List_Size - 1;
               end;
            end if;
         end;
      end loop;
      
      return Temp_List(1);
   end Shortest_Superstring;

   -- Read DNA sequences from input
   procedure Read_DNA_Sequences(DNA_List : in out array of DNA_String) is
      Line : Unbounded_String;
      Index : Natural := 1;
   begin
      while not End_Of_File loop
         Get_Line(Line);
         if Length(Line) > 0 and then To_String(Line)(1) = '>' then
            -- This is a header, skip it
            null;
         else
            DNA_List(Index) := (Sequence => Line);
            Index := Index + 1;
         end if;
      end loop;
   end Read_DNA_Sequences;

begin
   -- Example usage with sample data
   declare
      Sequences : array(1 .. 4) of DNA_String;
      Result : DNA_String;
   begin
      -- Sample input sequences (replace with actual reading from file)
      Sequences(1) := (Sequence => To_Unbounded_String("AACCTTGG"));
      Sequences(2) := (Sequence => To_Unbounded_String("TTGGAACT"));
      Sequences(3) := (Sequence => To_Unbounded_String("GGAATTCG"));
      Sequences(4) := (Sequence => To_Unbounded_String("CGAATTCG"));
      
      Result := Shortest_Superstring(Sequences);
      Put_Line("Shortest Superstring:");
      Put(Result);
   end;
   
end Genome_Assembly;
```

## Key Features of the Solution

1. **DNA_String Type**: Custom type to handle DNA sequences with proper string operations
2. **Overlap Calculation**: Computes maximum overlap between suffix of one string and prefix of another
3. **Merge Function**: Properly merges two strings by overlapping them maximally
4. **Greedy Approach**: Repeatedly finds the best pair to merge until one superstring remains
5. **Array Manipulation**: Handles dynamic removal of elements during the assembly process

## Time Complexity
- O(n² × m) where n is the number of sequences and m is the average sequence length
- This approach works well for moderate-sized inputs typical in Rosalind problems

## Usage Notes
To use this with actual Rosalind input files, you would need to:
1. Modify the `Read_DNA_Sequences` procedure to read from a file
2. Parse FASTA format properly
3. Adjust array sizes based on input data

The solution provides a solid foundation that can be adapted for the specific input format required by Rosalind's Genome Assembly problem.