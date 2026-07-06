# Rosalind Problem: Isolating Symbols in Alignments (Ada Solution)

## Problem Understanding

The task is to find all unique symbols that appear in a multiple sequence alignment and return them in alphabetical order.

## Solution Approach

1. Read input sequences from standard input
2. Collect all unique characters from all sequences
3. Sort the characters alphabetically
4. Output the sorted unique characters

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

procedure Isolating_Symbols_in_Alignments is
   
   type Sequence is array (Positive range <>) of Character;
   
   -- Vector to store unique symbols
   package Symbol_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Character);
   
   Symbols : Symbol_Vector.Vector;
   
   -- Function to check if character already exists in vector
   function Contains(Symbols : Symbol_Vector.Vector; C : Character) return Boolean is
   begin
      for I in Symbols.First_Index .. Symbols.Last_Index loop
         if Symbols.Element(I) = C then
            return True;
         end if;
      end loop;
      return False;
   end Contains;
   
   -- Function to insert character if not already present
   procedure Insert_Unique(Symbols : in out Symbol_Vector.Vector; C : Character) is
   begin
      if not Contains(Symbols, C) then
         Symbols.Append(C);
      end if;
   end Insert_Unique;
   
   -- Simple bubble sort for sorting characters
   procedure Sort_Characters(Chars : in out Symbol_Vector.Vector) is
      Temp : Character;
      Changed : Boolean;
   begin
      loop
         Changed := False;
         for I in Chars.First_Index .. Chars.Last_Index - 1 loop
            if Chars.Element(I) > Chars.Element(I + 1) then
               Temp := Chars.Element(I);
               Chars.Replace_Element(I, Chars.Element(I + 1));
               Chars.Replace_Element(I + 1, Temp);
               Changed := True;
            end if;
         end loop;
         exit when not Changed;
      end loop;
   end Sort_Characters;
   
   -- Read sequences and collect symbols
   procedure Process_Alignment is
      Line : Unbounded_String;
      C : Character;
   begin
      loop
         begin
            Line := Get_Line;
            if Length(Line) = 0 then
               exit;
            end if;
            
            for I in 1 .. Length(Line) loop
               C := To_Character(Substring(Line, I, I));
               Insert_Unique(Symbols, C);
            end loop;
         exception
            when End_Error =>
               exit;
         end;
      end loop;
   end Process_Alignment;
   
begin
   -- Process all input sequences
   Process_Alignment;
   
   -- Sort symbols alphabetically
   Sort_Characters(Symbols);
   
   -- Output results
   for I in Symbols.First_Index .. Symbols.Last_Index loop
      Put(Symbols.Element(I));
   end loop;
   New_Line;
   
end Isolating_Symbols_in_Alignments;
```

## Alternative Implementation (More Efficient)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Isolating_Symbols_in_Alignments is
   
   -- Vector to store unique symbols
   package Symbol_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Character);
   
   Symbols : Symbol_Vector.Vector;
   
   -- Set to track which characters we've seen
   type Character_Set is array (Character) of Boolean;
   
   Seen : Character_Set := (others => False);
   
begin
   -- Read all input lines
   declare
      Line : Unbounded_String;
      C : Character;
   begin
      loop
         begin
            Line := Get_Line;
            if Length(Line) = 0 then
               exit;
            end if;
            
            for I in 1 .. Length(Line) loop
               C := To_Character(Substring(Line, I, I));
               if not Seen(C) then
                  Seen(C) := True;
                  Symbols.Append(C);
               end if;
            end loop;
         exception
            when End_Error =>
               exit;
         end;
      end loop;
   end;
   
   -- Sort symbols using built-in sorting (if available)
   -- For simplicity, we'll do manual bubble sort
   declare
      Temp : Character;
      Changed : Boolean;
   begin
      loop
         Changed := False;
         for I in Symbols.First_Index .. Symbols.Last_Index - 1 loop
            if Symbols.Element(I) > Symbols.Element(I + 1) then
               Temp := Symbols.Element(I);
               Symbols.Replace_Element(I, Symbols.Element(I + 1));
               Symbols.Replace_Element(I + 1, Temp);
               Changed := True;
            end if;
         end loop;
         exit when not Changed;
      end loop;
   end;
   
   -- Output results
   for I in Symbols.First_Index .. Symbols.Last_Index loop
      Put(Symbols.Element(I));
   end loop;
   New_Line;
   
end Isolating_Symbols_in_Alignments;
```

## How to Compile and Run

```bash
# Compile the program
gnatmake isolating_symbols.adb

# Run with input redirection or pipe
./isolating_symbols < input.txt
```

## Example Input/Output

**Input:**
```
ACGTACGT
TGCATGCA
CGATCGAT
```

**Output:**
```
ACGT
```

The solution reads all sequences, collects unique characters (A, C, G, T), sorts them alphabetically, and outputs them as a single string.