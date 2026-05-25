# Rosalind Problem: Constructing a De Bruijn Graph

## Problem Statement
Given a collection of DNA strings, construct the de Bruijn graph of order k.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure De_Bruijn_Graph is
   
   package String_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Unbounded_String);
   
   package String_Hash_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Unbounded_String,
      Element_Type => String_Vector.Vector,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   
   type DNA_String is array (Positive range <>) of Character;
   
   function Prefix(S : Unbounded_String; k : Positive) return Unbounded_String is
      Result : Unbounded_String;
   begin
      if Length(S) < k then
         return S;
      else
         return Head(S, k);
      end if;
   end Prefix;
   
   function Suffix(S : Unbounded_String; k : Positive) return Unbounded_String is
      Result : Unbounded_String;
   begin
      if Length(S) < k then
         return S;
      else
         return Tail(S, k);
      end if;
   end Suffix;
   
   procedure Print_De_Bruijn_Graph(Reads : String_Vector.Vector; k : Positive) is
      Graph : String_Hash_Map.Map;
      Prefixes : array (Positive range <>) of Unbounded_String;
      Suffixes : array (Positive range <>) of Unbounded_String;
      Prefix_Count : Positive := 0;
      Suffix_Count : Positive := 0;
      
      procedure Add_To_Map(Key : Unbounded_String; Value : Unbounded_String) is
         Values : String_Vector.Vector;
      begin
         if String_Hash_Map.Has_Element(Graph, Key) then
            Values := String_Hash_Map.Element(Graph, Key);
         else
            Values := String_Vector.Empty_Vector;
         end if;
         
         String_Vector.Append(Values, Value);
         String_Hash_Map.Replace_Element(Graph, Key, Values);
      end Add_To_Map;
      
   begin
      -- Process all reads
      for I in 1 .. String_Vector.Length(Reads) loop
         declare
            Read : constant Unbounded_String := String_Vector.Element(Reads, I);
            Prefix_K : Unbounded_String;
            Suffix_K : Unbounded_String;
         begin
            if Length(Read) >= k then
               Prefix_K := Prefix(Read, k - 1);
               Suffix_K := Suffix(Read, k - 1);
               
               Add_To_Map(Prefix_K, Suffix_K);
            end if;
         end;
      end loop;
      
      -- Print the adjacency list
      for Key in String_Hash_Map.Iterate(Graph) loop
         declare
            Key_String : constant Unbounded_String := String_Hash_Map.Key(Key);
            Values : constant String_Vector.Vector := String_Hash_Map.Element(Key);
         begin
            Put(Key_String);
            Put(" -> ");
            
            for J in 1 .. String_Vector.Length(Values) loop
               if J > 1 then
                  Put(", ");
               end if;
               Put(String_Vector.Element(Values, J));
            end loop;
            
            New_Line;
         end;
      end loop;
   end Print_De_Bruijn_Graph;
   
   procedure Read_Reads(Reads : in out String_Vector.Vector) is
      Line : Unbounded_String;
   begin
      while not End_Of_File loop
         Line := Get_Line;
         if Length(Line) > 0 then
            String_Vector.Append(Reads, Line);
         end if;
      end loop;
   end Read_Reads;
   
   Reads : String_Vector.Vector;
   K : constant Positive := 3; -- k-mer length
begin
   Read_Reads(Reads);
   Print_De_Bruijn_Graph(Reads, K);
end De_Bruijn_Graph;
```

## Explanation

This Ada solution constructs a de Bruijn graph from DNA sequences:

1. **Data Structures**:
   - Uses `String_Vector` to store input DNA reads
   - Uses `String_Hash_Map` to represent the adjacency list of the graph

2. **Key Functions**:
   - `Prefix`: Extracts the first k-1 characters of a string
   - `Suffix`: Extracts the last k-1 characters of a string
   - `Print_De_Bruijn_Graph`: Constructs and prints the adjacency list

3. **Algorithm**:
   - For each DNA read, extract its prefix (first k-1 characters) and suffix (last k-1 characters)
   - Create edges from prefix to suffix in the graph
   - Print the adjacency list representation

4. **Output Format**:
   - Each line shows a prefix followed by " -> " and its connected suffixes
   - Multiple suffixes are separated by commas

## Sample Input/Output

**Input**:
```
ACG
CGT
GTA
TAC
```

**Output**:
```
AC -> CG
CG -> GT
GT -> TA
TA -> AC
```

The solution handles the de Bruijn graph construction by creating a mapping from k-1 length prefixes to their corresponding k-1 length suffixes, forming the adjacency relationships in the graph.

