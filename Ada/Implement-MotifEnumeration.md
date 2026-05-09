# Rosalind Problem: Implement_MotifEnumeration in Ada

## Problem Description
Given a collection of strings and integers k and d, find all k-mers that appear in every string with at most d mismatches.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Implement_MotifEnumeration is
   
   type String_Vector is array (Positive range <>) of Unbounded_String;
   
   function Hamming_Distance(s1, s2 : Unbounded_String) return Natural is
      count : Natural := 0;
      len   : constant Natural := Length(s1);
   begin
      for i in 1..len loop
         if Element(s1, i) /= Element(s2, i) then
            count := count + 1;
         end if;
      end loop;
      return count;
   end Hamming_Distance;
   
   function Is_Mismatch(s1, s2 : Unbounded_String; d : Natural) return Boolean is
   begin
      return Hamming_Distance(s1, s2) <= d;
   end Is_Mismatch;
   
   function Get_Kmers(s : Unbounded_String; k : Natural) return String_Vector is
      kmers : String_Vector(1..Length(s) - k + 1);
      len   : constant Natural := Length(s);
   begin
      for i in 1..len - k + 1 loop
         kmers(i) := To_Unbounded_String(To_String(s)(i..i+k-1));
      end loop;
      return kmers;
   end Get_Kmers;
   
   function Get_All_Kmers(s : Unbounded_String; k : Natural) return String_Vector is
      kmers : String_Vector(1..Length(s) - k + 1);
      len   : constant Natural := Length(s);
   begin
      for i in 1..len - k + 1 loop
         kmers(i) := To_Unbounded_String(To_String(s)(i..i+k-1));
      end loop;
      return kmers;
   end Get_All_Kmers;
   
   function Get_All_Kmers_From_List(strings : String_Vector; k : Natural) return String_Vector is
      all_kmers : String_Vector(1..1000); -- Assuming maximum capacity
      count     : Natural := 0;
   begin
      for i in strings'First..strings'Last loop
         declare
            kmers : constant String_Vector := Get_All_Kmers(strings(i), k);
         begin
            for j in kmers'First..kmers'Last loop
               count := count + 1;
               all_kmers(count) := kmers(j);
            end loop;
         end;
      end loop;
      return all_kmers(1..count);
   end Get_All_Kmers_From_List;
   
   function Get_Possible_Kmers(k : Natural) return String_Vector is
      -- Generate all possible k-mers
      result : String_Vector(1..2**k); -- Conservative estimate
      count  : Natural := 0;
      procedure Generate_Mers(current : Unbounded_String; remaining : Natural) is
      begin
         if remaining = 0 then
            count := count + 1;
            result(count) := current;
         else
            Generate_Mers(To_Unbounded_String(To_String(current) & 'A'), remaining - 1);
            Generate_Mers(To_Unbounded_String(To_String(current) & 'C'), remaining - 1);
            Generate_Mers(To_Unbounded_String(To_String(current) & 'G'), remaining - 1);
            Generate_Mers(To_Unbounded_String(To_String(current) & 'T'), remaining - 1);
         end if;
      end Generate_Mers;
   begin
      Generate_Mers(To_Unbounded_String(""), k);
      return result(1..count);
   end Get_Possible_Kmers;
   
   function Is_Motif_Present(kmer : Unbounded_String; strings : String_Vector; d : Natural) return Boolean is
      found : Boolean := True;
   begin
      for i in strings'First..strings'Last loop
         declare
            kmers : constant String_Vector := Get_All_Kmers(strings(i), Length(kmer));
            match : Boolean := False;
         begin
            for j in kmers'First..kmers'Last loop
               if Is_Mismatch(kmer, kmers(j), d) then
                  match := True;
                  exit;
               end if;
            end loop;
            if not match then
               found := False;
               exit;
            end if;
         end;
      end loop;
      return found;
   end Is_Motif_Present;
   
   function Motif_Enumeration(strings : String_Vector; k : Natural; d : Natural) return String_Vector is
      possible_kmers : constant String_Vector := Get_Possible_Kmers(k);
      result         : String_Vector(1..1000);
      count          : Natural := 0;
   begin
      for i in possible_kmers'First..possible_kmers'Last loop
         if Is_Motif_Present(possible_kmers(i), strings, d) then
            count := count + 1;
            result(count) := possible_kmers(i);
         end if;
      end loop;
      return result(1..count);
   end Motif_Enumeration;
   
   -- Example usage
   procedure Test_Motif_Enumeration is
      strings : String_Vector(1..3);
      k       : constant Natural := 3;
      d       : constant Natural := 1;
      result  : String_Vector(1..100);
      count   : Natural;
   begin
      strings(1) := To_Unbounded_String("ATTTGGC");
      strings(2) := To_Unbounded_String("TGCCTTA");
      strings(3) := To_Unbounded_String("GGATCCC");
      
      result := Motif_Enumeration(strings, k, d);
      count := result'Last;
      
      Put_Line("Motifs found:");
      for i in 1..count loop
         Put_Line(To_String(result(i)));
      end loop;
   end Test_Motif_Enumeration;
   
begin
   Test_Motif_Enumeration;
end Implement_MotifEnumeration;
```

## Explanation

This Ada implementation solves the motif enumeration problem by:

1. **Hamming Distance Calculation**: Computes the number of mismatches between two strings
2. **Mismatch Check**: Determines if two strings have at most d mismatches
3. **K-mer Generation**: Extracts all k-mers from a given string
4. **Motif Enumeration**: Finds all k-mers that appear in every string with at most d mismatches

## Key Features

- **String Handling**: Uses `Unbounded_String` for flexible string operations
- **Recursion**: Implements recursive generation of possible k-mers
- **Vector Operations**: Uses arrays to handle collections of strings
- **Modular Design**: Breaks down the problem into smaller, reusable functions

## Time Complexity
- O(4^k × n × k) where n is the total length of all strings
- The algorithm generates all possible k-mers and checks each against all input strings

## Space Complexity
- O(4^k) for storing possible k-mers
- O(n × k) for storing all k-mers from input strings

This solution handles the core requirements of the Rosalind problem while maintaining clean Ada programming practices.

