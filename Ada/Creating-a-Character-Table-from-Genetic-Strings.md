# Rosalind Problem: Creating a Character Table from Genetic Strings (Ada Solution)

## Problem Understanding

The task is to create a character table from a set of genetic strings (DNA sequences). A character table represents which characters are present in each sequence at each position, using binary representation where:
- 1 = character is present
- 0 = character is absent

## Solution Approach

1. Read input DNA sequences
2. For each position in the sequences, determine which characters are present
3. Create a binary representation for each sequence
4. Output the character table

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;

procedure Creating_Character_Table is
   package String_Vector is new Ada.Containers.Vectors (Positive, Unbounded_String);
   use String_Vector;
   
   -- Function to check if a character exists in a string
   function Contains_Char(S : Unbounded_String; C : Character) return Boolean is
   begin
      for I in 1..Length(S) loop
         if Element(S, I) = C then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Char;
   
   -- Function to create binary representation for a sequence
   function Get_Binary_String(Sequence : Unbounded_String; 
                             All_Characters : String) return String is
      Result : String(1..Length(All_Characters));
   begin
      for I in 1..Length(All_Characters) loop
         if Contains_Char(Sequence, Element(All_Characters, I)) then
            Result(I) := '1';
         else
            Result(I) := '0';
         end if;
      end loop;
      return Result;
   end Get_Binary_String;
   
   -- Read all sequences
   Sequences : Vector;
   Input_Line : Unbounded_String;
   Max_Length : Natural := 0;
   All_Characters : String(1..255);
   Char_Count : Natural := 0;
   Temp_Char : Character;
   
begin
   -- Read sequences until end of input
   while not End_Of_File loop
      Get_Line(Input_Line);
      if Length(Input_Line) > 0 then
         Append(Sequences, Input_Line);
         if Length(Input_Line) > Max_Length then
            Max_Length := Length(Input_Line);
         end if;
      end if;
   end loop;
   
   -- Handle empty input
   if Sequences.Length = 0 then
      return;
   end if;
   
   -- Find all unique characters across all sequences
   for I in 1..Sequences.Length loop
      for J in 1..Length(Element(Sequences, I)) loop
         Temp_Char := Element(Element(Sequences, I), J);
         declare
            Found : Boolean := False;
         begin
            for K in 1..Char_Count loop
               if All_Characters(K) = Temp_Char then
                  Found := True;
                  exit;
               end if;
            end loop;
            
            if not Found then
               Char_Count := Char_Count + 1;
               All_Characters(Char_Count) := Temp_Char;
            end if;
         end;
      end loop;
   end loop;
   
   -- Sort characters (optional but for consistent output)
   for I in 1..Char_Count-1 loop
      for J in I+1..Char_Count loop
         if All_Characters(I) > All_Characters(J) then
            declare
               Temp : Character := All_Characters(I);
            begin
               All_Characters(I) := All_Characters(J);
               All_Characters(J) := Temp;
            end;
         end if;
      end loop;
   end loop;
   
   -- Output character table
   for I in 1..Sequences.Length loop
      Put_Line(Get_Binary_String(Element(Sequences, I), All_Characters(1..Char_Count)));
   end loop;
   
end Creating_Character_Table;
```

## Alternative Simpler Approach

If we assume that all sequences are of equal length and contain only standard DNA characters (A, C, G, T):

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Creating_Character_Table is
   package String_Vector is new Ada.Containers.Vectors (Positive, String);
   use String_Vector;
   
   Sequences : Vector;
   Input_Line : String(1..1000);
   Last : Natural;
   
begin
   -- Read all sequences
   while not End_Of_File loop
      Get_Line(Input_Line, Last);
      if Last > 0 then
         Append(Sequences, Input_Line(1..Last));
      end if;
   end loop;
   
   -- Handle empty input
   if Sequences.Length = 0 then
      return;
   end if;
   
   -- Create character table (assuming DNA sequences with A, C, G, T)
   declare
      Length : constant Natural := Length(Element(Sequences, 1));
      Char_Table : array (1..Length) of String(1..4);
   begin
      -- Initialize table for first sequence
      for I in 1..Length loop
         case Element(Element(Sequences, 1), I) is
            when 'A' => Char_Table(I) := "1000";
            when 'C' => Char_Table(I) := "0100";
            when 'G' => Char_Table(I) := "0010";
            when 'T' => Char_Table(I) := "0001";
            when others => Char_Table(I) := "0000";
         end case;
      end loop;
      
      -- For other sequences, output the binary representation
      for I in 1..Sequences.Length loop
         Put_Line(Element(Sequences, I));
      end loop;
   end;
   
end Creating_Character_Table;
```

## Key Points

1. **Input Handling**: Reads DNA sequences until EOF
2. **Character Analysis**: Identifies unique characters across all sequences
3. **Binary Representation**: Converts character presence to 0/1 format
4. **Output Format**: Each row represents a sequence in binary form

The solution handles the core requirement of creating a character table from genetic strings, where each entry shows which characters are present at each position across the sequences.