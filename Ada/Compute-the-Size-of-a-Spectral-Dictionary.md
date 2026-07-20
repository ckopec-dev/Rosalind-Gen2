# Rosalind Problem: Compute the Size of a Spectral Dictionary

## Problem Statement
Given a string `s` and an integer `k`, we want to compute the size of the spectral dictionary of all substrings of length `k` in `s`.

The spectral dictionary is defined as the set of all substrings of length `k` that appear in the string `s`, where each substring is considered unique regardless of its position.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Compute_Spectral_Dictionary_Size is
   
   package String_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   
   type Substring_Vector is array (Natural range <>) of Unbounded_String;
   
   function Get_Substrings(S : Unbounded_String; K : Natural) return Substring_Vector is
      Length : constant Natural := Length(S);
      Result : Substring_Vector(0..Length-K);
      I : Natural;
   begin
      for I in 0..Length-K loop
         Result(I) := To_Unbounded_String(Slice(S, I+1, I+K));
      end loop;
      return Result;
   end Get_Substrings;
   
   function Count_Unique_Substrings(S : Unbounded_String; K : Natural) return Natural is
      Substrings : constant Substring_Vector := Get_Substrings(S, K);
      Length : constant Natural := Length(S);
      Unique_Count : Natural := 0;
      Is_Unique : Boolean;
      
      -- Vector to store unique substrings
      Unique_List : String_Vector.Vector;
   begin
      for I in 0..Length-K loop
         Is_Unique := True;
         
         -- Check if substring already exists in our unique list
         for J in 0..Unique_List.Length-1 loop
            if Equal(Unique_List.Element(J), Substrings(I)) then
               Is_Unique := False;
               exit;
            end if;
         end loop;
         
         if Is_Unique then
            Unique_List.Append(Substrings(I));
            Unique_Count := Unique_Count + 1;
         end if;
      end loop;
      
      return Unique_Count;
   end Count_Unique_Substrings;
   
   -- Main program
   S : Unbounded_String := To_Unbounded_String("ACGTACGT");
   K : constant Natural := 3;
   Result : Natural;
   
begin
   Put_Line("Input string: " & To_String(S));
   Put_Line("Substring length k: " & Integer'Image(K));
   
   Result := Count_Unique_Substrings(S, K);
   
   Put_Line("Size of spectral dictionary: " & Natural'Image(Result));
   
   -- Test with example from Rosalind
   S := To_Unbounded_String("ACGTACGT");
   K := 3;
   Result := Count_Unique_Substrings(S, K);
   Put_Line("Example result: " & Natural'Image(Result));
end Compute_Spectral_Dictionary_Size;
```

## Alternative Implementation (More Efficient)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hash_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Compute_Spectral_Dictionary_Size_Efficient is
   
   function Hash(S : Unbounded_String) return Natural is
      -- Simple hash function for strings
      Result : Natural := 0;
      Length : constant Natural := Length(S);
   begin
      for I in 1..Length loop
         Result := Result * 31 + Character'Pos(Element(S, I));
      end loop;
      return Result;
   end Hash;
   
   package Substring_Map is new Ada.Containers.Indefinite_Hash_Maps
     (Key_Type => Unbounded_String,
      Element_Type => Boolean,
      Hash => Hash,
      Equivalent_Keys => "=");
   
   function Count_Unique_Substrings(S : Unbounded_String; K : Natural) return Natural is
      Length : constant Natural := Length(S);
      Map : Substring_Map.Map;
      Count : Natural := 0;
   begin
      if Length < K then
         return 0;
      end if;
      
      for I in 0..Length-K loop
         declare
            Substring : constant Unbounded_String := 
               To_Unbounded_String(Slice(S, I+1, I+K));
         begin
            if not Map.Contains(Substring) then
               Map.Insert(Substring, True);
               Count := Count + 1;
            end if;
         end;
      end loop;
      
      return Count;
   end Count_Unique_Substrings;
   
begin
   -- Test with Rosalind example
   declare
      S : constant Unbounded_String := To_Unbounded_String("ACGTACGT");
      K : constant Natural := 3;
      Result : Natural;
   begin
      Result := Count_Unique_Substrings(S, K);
      Put_Line("String: " & To_String(S));
      Put_Line("k = " & Integer'Image(K));
      Put_Line("Size of spectral dictionary: " & Natural'Image(Result));
   end;
end Compute_Spectral_Dictionary_Size_Efficient;
```

## Explanation

The problem asks us to find the size of a spectral dictionary, which is essentially counting unique substrings of length `k` in a given string.

### Key Points:

1. **Input**: A string `s` and integer `k`
2. **Output**: The count of unique substrings of length `k` 
3. **Approach**:
   - Extract all substrings of length `k` from the input string
   - Count how many unique substrings there are
   - For example, with "ACGTACGT" and k=3: 
     - Substrings: ACG, CGT, GTA, TAC, ACG, CGT, GTA, TAC
     - Unique substrings: ACG, CGT, GTA, TAC
     - Count: 4

### Time Complexity:
- O(n*k) where n is the length of the string and k is the substring length
- The efficient version uses hash maps for O(1) average lookup time

### Space Complexity:
- O(n*k) to store all unique substrings

The second implementation is more efficient as it uses a hash map to track unique substrings, avoiding the need to manually check each new substring against all previously found ones.