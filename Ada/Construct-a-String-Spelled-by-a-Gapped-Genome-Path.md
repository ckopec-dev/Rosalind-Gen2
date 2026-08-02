# Rosalind Problem: Construct a String Spelled by a Gapped Genome Path

## Problem Understanding

We need to reconstruct a string from a gapped genome path, where each element consists of two parts (a and b) separated by a gap of length k.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Construct_String_Spelled_By_Gapped_Genome_Path is
   
   type Gapped_Pair is record
      First : Unbounded_String;
      Second : Unbounded_String;
   end record;
   
   type Gapped_Path is array (Positive range <>) of Gapped_Pair;
   
   -- Function to get the first part of a gapped pair
   function Get_First(Pair : Gapped_Pair) return Unbounded_String is
   begin
      return Pair.First;
   end Get_First;
   
   -- Function to get the second part of a gapped pair
   function Get_Second(Pair : Gapped_Pair) return Unbounded_String is
   begin
      return Pair.Second;
   end Get_Second;
   
   -- Main reconstruction function
   function Reconstruct_String(Path : Gapped_Path; k : Integer) return Unbounded_String is
      First_String : Unbounded_String := Null_Unbounded_String;
      Second_String : Unbounded_String := Null_Unbounded_String;
      Result : Unbounded_String := Null_Unbounded_String;
      i : Integer;
   begin
      -- Extract first parts of all pairs
      for i in Path'First .. Path'Last loop
         First_String := First_String & Get_First(Path(i));
      end loop;
      
      -- Extract second parts of all pairs
      for i in Path'First .. Path'Last loop
         Second_String := Second_String & Get_Second(Path(i));
      end loop;
      
      -- Reconstruct the final string by overlapping
      -- The first part contributes k characters to the overlap
      -- The second part contributes k characters to the overlap
      Result := First_String;
      
      -- Append the second string starting from position k+1
      for i in (k + 1) .. Length(Second_String) loop
         Result := Result & Element(Second_String, i);
      end loop;
      
      return Result;
   end Reconstruct_String;
   
   -- Read input function
   procedure Read_Gapped_Path(Path : out Gapped_Path; n : Integer) is
      Line : Unbounded_String;
      First_Part, Second_Part : Unbounded_String;
      i : Integer := 1;
   begin
      for i in 1 .. n loop
         Put_Line("Enter gapped pair " & Integer'image(i) & ":");
         Get_Line(Line);
         
         -- Parse the input (format: "first_part|second_part")
         declare
            Separator_Pos : Natural := Index(Line, '|');
         begin
            if Separator_Pos > 0 then
               First_Part := Slice(Line, 1, Separator_Pos - 1);
               Second_Part := Slice(Line, Separator_Pos + 1, Length(Line));
               Path(i).First := First_Part;
               Path(i).Second := Second_Part;
            else
               -- If no separator, treat entire string as first part
               Path(i).First := Line;
               Path(i).Second := Null_Unbounded_String;
            end if;
         end;
      end loop;
   end Read_Gapped_Path;
   
   -- Print result function
   procedure Print_Result(Result : Unbounded_String) is
   begin
      Put_Line("Reconstructed string:");
      Put_Line(To_String(Result));
   end Print_Result;
   
   -- Example usage
   procedure Example_Usage is
      Path : Gapped_Path(1..4);
      k : constant Integer := 2;
      Result : Unbounded_String;
   begin
      -- Example from Rosalind problem:
      -- Input: (GACC|CGCC), (ACCG|GCCG), (CCGA|CCTG), (CGAG|CTGG)
      Path(1).First := To_Unbounded_String("GACC");
      Path(1).Second := To_Unbounded_String("CGCC");
      Path(2).First := To_Unbounded_String("ACCG");
      Path(2).Second := To_Unbounded_String("GCCG");
      Path(3).First := To_Unbounded_String("CCGA");
      Path(3).Second := To_Unbounded_String("CCTG");
      Path(4).First := To_Unbounded_String("CGAG");
      Path(4).Second := To_Unbounded_String("CTGG");
      
      Result := Reconstruct_String(Path, k);
      Print_Result(Result);
   end Example_Usage;
   
begin
   Put_Line("Construct a String Spelled by a Gapped Genome Path");
   Put_Line("===================================================");
   
   Example_Usage;
   
end Construct_String_Spelled_By_Gapped_Genome_Path;
```

## Key Concepts

1. **Gapped Genome Path**: A sequence of pairs where each pair consists of two strings separated by a gap of length k
2. **Reconstruction Process**:
   - Extract first parts from all pairs to form the first string
   - Extract second parts from all pairs to form the second string
   - Overlap these strings appropriately using the gap information

## Algorithm Steps

1. Parse input gapped genome path into pairs of strings
2. Extract all first parts and concatenate them
3. Extract all second parts and concatenate them  
4. Combine the two strings by overlapping appropriately
5. Return the final reconstructed string

## Time Complexity
- O(n × m) where n is the number of pairs and m is the average length of each part

## Space Complexity  
- O(n × m) for storing the input and result strings

This solution handles the core requirements of reconstructing a string from gapped genome paths as specified in the Rosalind problem.