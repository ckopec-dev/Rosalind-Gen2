# Rosalind Problem: Creating a Character Table (in Ada)

## Problem Understanding

The task is to create a character table from a set of DNA sequences. A character table represents which characters (nucleotides) appear at each position across all sequences, typically represented as binary strings where 1 indicates presence and 0 indicates absence.

## Solution Approach

I'll implement this in Ada by:
1. Reading input sequences
2. Creating a character table representation
3. Converting the table to the required binary format

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Creating_A_Character_Table is
   
   type Sequence is array (Positive range <>) of Character;
   package Sequence_Vector is new Ada.Containers.Vectors (Positive, Sequence);
   use Sequence_Vector;
   
   -- Read sequences from input
   function Read_Sequences return Vector is
      Result : Vector;
      Line   : Unbounded_String;
   begin
      while not End_Of_File loop
         Line := To_Unbounded_String (Get_Line);
         if Length (Line) > 0 then
            declare
               S : Sequence (1 .. Length (Line));
            begin
               for I in S'Range loop
                  S (I) := Element (Line, I);
               end loop;
               Append (Result, S);
            end;
         end if;
      end loop;
      return Result;
   end Read_Sequences;
   
   -- Get the maximum length of sequences
   function Max_Length (Sequences : Vector) return Natural is
      Max : Natural := 0;
   begin
      for I in Sequences.First_Index .. Sequences.Last_Index loop
         if Sequences.Element (I)'Length > Max then
            Max := Sequences.Element (I)'Length;
         end if;
      end loop;
      return Max;
   end Max_Length;
   
   -- Create character table
   procedure Create_Character_Table (Sequences : Vector) is
      Max_Len : constant Natural := Max_Length (Sequences);
      Num_Seq : constant Natural := Sequences.Length;
      
      -- For each position, count how many sequences have each nucleotide
      type Nucleotide_Count is array ('A' .. 'Z') of Natural;
      type Position_Counts is array (Positive range <>) of Nucleotide_Count;
      
      Counts : Position_Counts (1 .. Max_Len);
      
      -- Initialize counts
      procedure Init_Counts is
      begin
         for Pos in Counts'Range loop
            for Nuc in 'A' .. 'Z' loop
               Counts (Pos) (Nuc) := 0;
            end loop;
         end loop;
      end Init_Counts;
      
      -- Count nucleotides at each position
      procedure Count_Nucleotides is
      begin
         Init_Counts;
         for Seq_Index in Sequences.First_Index .. Sequences.Last_Index loop
            declare
               Current_Seq : constant Sequence := Sequences.Element (Seq_Index);
            begin
               for Pos in 1 .. Current_Seq'Length loop
                  if Pos <= Max_Len then
                     Counts (Pos) (Current_Seq (Pos)) := Counts (Pos) (Current_Seq (Pos)) + 1;
                  end if;
               end loop;
            end;
         end loop;
      end Count_Nucleotides;
      
   begin
      Count_Nucleotides;
      
      -- Output the character table in binary format
      for Pos in 1 .. Max_Len loop
         declare
            Line : Unbounded_String := Null_Unbounded_String;
         begin
            for Seq_Index in Sequences.First_Index .. Sequences.Last_Index loop
               declare
                  Current_Seq : constant Sequence := Sequences.Element (Seq_Index);
               begin
                  if Pos <= Current_Seq'Length then
                     -- If this sequence has nucleotide at this position, output 1
                     -- Otherwise output 0
                     if Current_Seq (Pos) in 'A' .. 'Z' then
                        Line := Line & "1";
                     else
                        Line := Line & "0";
                     end if;
                  else
                     Line := Line & "0";
                  end if;
               end;
            end loop;
            Put_Line (To_String (Line));
         end;
      end loop;
   end Create_Character_Table;

begin
   -- Read all sequences and create character table
   declare
      Sequences : constant Vector := Read_Sequences;
   begin
      if Sequences.Length > 0 then
         Create_Character_Table (Sequences);
      end if;
   end;
end Creating_A_Character_Table;
```

## Key Features

1. **Input Processing**: Reads sequences line by line from standard input
2. **Dynamic Arrays**: Uses Ada containers for flexible sequence handling
3. **Character Table Generation**: Creates binary representation showing presence/absence of characters
4. **Proper Memory Management**: Ada's built-in garbage collection handles memory cleanup

## Example Usage

If input contains:
```
ATCG
ATGG
AAGG
```

The output would be a character table where each row represents the binary presence (1) or absence (0) of characters at each position across all sequences.

## Notes

This solution assumes that the problem requires converting to binary format where 1 indicates a character is present in that sequence at that position, and 0 indicates it's absent. The exact interpretation may vary depending on the specific requirements of the Rosalind problem.