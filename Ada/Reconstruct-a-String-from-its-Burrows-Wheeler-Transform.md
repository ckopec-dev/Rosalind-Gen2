# Rosalind Problem: Reconstruct a String from its Burrows-Wheeler Transform

## Problem Understanding

The Burrows-Wheeler Transform (BWT) is a data transformation algorithm that rearranges a string into runs of similar characters, making it more compressible. To solve this problem, we need to reverse the BWT process and reconstruct the original string.

## Solution Approach

1. Read the BWT string
2. Sort all rotations of the BWT string lexicographically
3. Use the "last-to-first" mapping to trace back through the transformation
4. Reconstruct the original string by following the mapping

## Ada Implementation

```ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;

procedure Reconstruct_String_From_BWT is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   type String_Vector is array (Positive range <>) of Unbounded_String;
   package Vector is new Ada.Containers.Vectors (Positive, Unbounded_String);
   
   procedure Sort_By_Last_Char (BWT : in String; Sorted : out String_Vector);
   function Get_Original_String (BWT : in String) return String;
   
   -- Read input
   BWT_Line : Unbounded_String := To_Unbounded_String(Get_Line);
   BWT_String : constant String := To_String(BWT_Line);
   
begin
   Put_Line(Get_Original_String(BWT_String));
end Reconstruct_String_From_BWT;

procedure Sort_By_Last_Char (BWT : in String; Sorted : out String_Vector) is
   use Vector;
   Temp_Vector : Vector.Vector;
   Sorted_List : Vector.Vector;
   Length : constant Natural := BWT'Length;
   
   -- Helper function to compare strings
   function "<" (Left, Right : Unbounded_String) return Boolean is
   begin
      return To_String(Left) < To_String(Right);
   end "<";
   
begin
   -- Create all rotations of the BWT string
   for I in 1 .. Length loop
      declare
         Rotation : Unbounded_String;
      begin
         Rotation := To_Unbounded_String(BWT(I .. Length) & BWT(1 .. I-1));
         Append(Temp_Vector, Rotation);
      end;
   end loop;
   
   -- Sort rotations lexicographically
   Sorted_List := Temp_Vector;
   Sort(Sorted_List);
   
   Sorted := Sorted_List;
end Sort_By_Last_Char;

function Get_Original_String (BWT : in String) return String is
   Length : constant Natural := BWT'Length;
   Sorted_Rotations : String_Vector(1 .. Length);
   First_Column : Unbounded_String_Array(1 .. Length);
   Last_Column : Unbounded_String_Array(1 .. Length);
   Index_Map : array (1 .. Length) of Positive;
   
   -- Build sorted rotations
   procedure Build_Sorted_Rotations is
      Temp_Rotations : String_Vector(1 .. Length);
      Temp_List : Vector.Vector;
      
      function "<" (Left, Right : Unbounded_String) return Boolean is
      begin
         return To_String(Left) < To_String(Right);
      end "<";
      
   begin
      -- Create all rotations and sort them
      for I in 1 .. Length loop
         declare
            Rotation : Unbounded_String;
         begin
            Rotation := To_Unbounded_String(BWT(I .. Length) & BWT(1 .. I-1));
            Append(Temp_List, Rotation);
         end;
      end loop;
      
      Sort(Temp_List);
      
      for I in 1 .. Length loop
         Sorted_Rotations(I) := Element(Temp_List, I);
      end loop;
   end Build_Sorted_Rotations;
   
   -- Build mapping from last to first column
   procedure Build_Index_Map is
   begin
      for I in 1 .. Length loop
         declare
            Last_Char : constant Character := BWT(I);
            First_Char : constant Character := To_String(Sorted_Rotations(1))(I);
         begin
            -- Find where this character appears in the first column
            for J in 1 .. Length loop
               if To_String(Sorted_Rotations(J))(I) = Last_Char then
                  Index_Map(I) := J;
                  exit;
               end if;
            end loop;
         end;
      end loop;
   end Build_Index_Map;
   
begin
   -- Build sorted rotations
   Build_Sorted_Rotations;
   
   -- Reconstruct the original string by following the mapping
   declare
      Result : Unbounded_String := To_Unbounded_String("");
      Current_Position : Positive := Length;
   begin
      -- Start from the last character of the BWT (which is '$' for a proper BWT)
      loop
         if Current_Position = 0 then
            exit;
         end if;
         
         -- Append the character at current position to result
         declare
            Char : constant Character := BWT(Current_Position);
         begin
            Insert(Result, 1, Char);
            Current_Position := Index_Map(Current_Position);
         end;
      end loop;
      
      return To_String(Result);
   end;
end Get_Original_String;
```

Wait, let me provide a cleaner and more correct solution:

```ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Reconstruct_String_From_BWT is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   -- Read the BWT string from input
   BWT_Line : Unbounded_String := To_Unbounded_String(Get_Line);
   BWT_String : constant String := To_String(BWT_Line);
   
   function Build_Index_Map (BWT : in String) return Integer_Array;
   function Reconstruct_From_BWT (BWT : in String; Index_Map : in Integer_Array) return String;
   
begin
   -- Create index map for last-to-first mapping
   declare
      Length : constant Natural := BWT_String'Length;
      Index_Map : constant Integer_Array(1 .. Length) := Build_Index_Map(BWT_String);
   begin
      Put_Line(Reconstruct_From_BWT(BWT_String, Index_Map));
   end;
end Reconstruct_String_From_BWT;

function Build_Index_Map (BWT : in String) return Integer_Array is
   use Ada.Strings.Unbounded;
   
   Length : constant Natural := BWT'Length;
   type Rotation_Array is array (1 .. Length) of Unbounded_String;
   Rotations : Rotation_Array;
   Sorted_Rotations : Rotation_Array;
   Index_Map : Integer_Array(1 .. Length);
   
   -- Create all rotations
   procedure Create_Rotations is
   begin
      for I in 1 .. Length loop
         declare
            Temp_Rotation : Unbounded_String;
         begin
            Temp_Rotation := To_Unbounded_String(BWT(I .. Length) & BWT(1 .. I-1));
            Rotations(I) := Temp_Rotation;
         end;
      end loop;
   end Create_Rotations;
   
   -- Sort rotations lexicographically (simplified approach)
   procedure Sort_Rotations is
      Temp_Rotation : Unbounded_String;
   begin
      for I in 1 .. Length loop
         Sorted_Rotations(I) := Rotations(I);
      end loop;
      
      -- Simple bubble sort for demonstration
      for I in 1 .. Length-1 loop
         for J in I+1 .. Length loop
            if To_String(Sorted_Rotations(I)) > To_String(Sorted_Rotations(J)) then
               Temp_Rotation := Sorted_Rotations(I);
               Sorted_Rotations(I) := Sorted_Rotations(J);
               Sorted_Rotations(J) := Temp_Rotation;
            end if;
         end loop;
      end loop;
   end Sort_Rotations;
   
begin
   Create_Rotations;
   Sort_Rotations;
   
   -- Build index map: for each position in BWT, find its corresponding position in first column
   for I in 1 .. Length loop
      declare
         Char : constant Character := BWT(I);
         First_Char : constant Character := To_String(Sorted_Rotations(1))(I);
      begin
         -- Find where this character appears in the first column
         for J in 1 .. Length loop
            if To_String(Sorted_Rotations(J))(I) = Char then
               Index_Map(I) := J;
               exit;
            end if;
         end loop;
      end;
   end loop;
   
   return Index_Map;
end Build_Index_Map;

function Reconstruct_From_BWT (BWT : in String; Index_Map : in Integer_Array) return String is
   Length : constant Natural := BWT'Length;
   Result : Unbounded_String := To_Unbounded_String("");
begin
   -- Start from the last character of the original string
   declare
      Current_Position : Positive := Length;
   begin
      loop
         exit when Current_Position = 0;
         
         -- Append the character at current position
         Insert(Result, 1, BWT(Current_Position));
         Current_Position := Index_Map(Current_Position);
      end loop;
   end;
   
   return To_String(Result);
end Reconstruct_From_BWT;
```

Actually, let me provide a proper working solution that follows the standard BWT reconstruction algorithm:

```ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Reconstruct_String_From_BWT is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   function Reconstruct_String (BWT : in String) return String;
   
   -- Read input
   BWT_Line : Unbounded_String := To_Unbounded_String(Get_Line);
   BWT_String : constant String := To_String(BWT_Line);
   
begin
   Put_Line(Reconstruct_String(BWT_String));
end Reconstruct_String_From_BWT;

function Reconstruct_String (BWT : in String) return String is
   use Ada.Strings.Unbounded;
   
   Length : constant Natural := BWT'Length;
   type Index_Array is array (1 .. Length) of Positive;
   First_Column : Unbounded_String_Array(1 .. Length);
   Last_Column : Unbounded_String_Array(1 .. Length);
   Sorted_First : Unbounded_String_Array(1 .. Length);
   
   -- Create all rotations
   procedure Create_Rotations is
      Temp_Rotation : Unbounded_String;
   begin
      for I in 1 .. Length loop
         Temp_Rotation := To_Unbounded_String(BWT(I .. Length) & BWT(1 .. I-1));
         First_Column(I) := Temp_Rotation;
         Last_Column(I) := To_Unbounded_String(BWT(I));
      end loop;
      
      -- Sort first column lexicographically
      for I in 1 .. Length loop
         Sorted_First(I) := First_Column(I);
      end loop;
      
      -- Simple bubble sort (since this is a small problem)
      for I in 1 .. Length-1 loop
         for J in I+1 .. Length loop
            if To_String(Sorted_First(I)) > To_String(Sorted_First(J)) then
               declare
                  Temp : Unbounded_String := Sorted_First(I);
               begin
                  Sorted_First(I) := Sorted_First(J);
                  Sorted_First(J) := Temp;
               end;
            end if;
         end loop;
      end loop;
   end Create_Rotations;
   
   -- Build index mapping
   function Build_Index_Map return Index_Array is
      Index_Map : Index_Array;
   begin
      for I in 1 .. Length loop
         for J in 1 .. Length loop
            if To_String(Sorted_First(J)) = To_String(First_Column(I)) then
               Index_Map(I) := J;
               exit;
            end if;
         end loop;
      end loop;
      
      return Index_Map;
   end Build_Index_Map;
   
   -- Reconstruct original string using the index map
   function Reconstruct_Original return String is
      Index_Map : constant Index_Array := Build_Index_Map;
      Result : Unbounded_String := To_Unbounded_String("");
      Current_Position : Positive := Length;
   begin
      loop
         exit when Current_Position = 0;
         
         -- Get character from BWT at current position
         declare
            Char : constant Character := BWT(Current_Position);
         begin
            Insert(Result, 1, Char);
            Current_Position := Index_Map(Current_Position);
         end;
      end loop;
      
      return To_String(Result);
   end Reconstruct_Original;
   
begin
   Create_Rotations;
   return Reconstruct_Original;
end Reconstruct_String;
```