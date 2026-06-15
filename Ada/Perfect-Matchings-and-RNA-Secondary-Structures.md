# Rosalind Problem: Perfect Matchings and RNA Secondary Structures

## Problem Understanding

We need to find the number of perfect matchings in an RNA string where:
- Each base pairs with its complement (A↔U, C↔G)
- A perfect matching means every base is paired exactly once
- We're given a string of length ≤ 100

This is essentially counting the number of ways to pair up nucleotides such that:
- A pairs with U
- C pairs with G
- Each position is matched exactly once

## Solution Approach

1. Count occurrences of each nucleotide
2. For perfect matching, we need equal numbers of A/U and C/G pairs
3. The number of perfect matchings follows the formula for pairing:
   - If we have n pairs of A's and U's, there are n! ways to pair them
   - Same for C's and G's
   - Total = (n_A! × n_U!) × (n_C! × n_G!)

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Perfect_Matchings is
   type RNA_String is array (Positive range <>) of Character;
   
   -- Function to compute factorial
   function Factorial(N : Natural) return Natural is
      Result : Natural := 1;
   begin
      for I in 2 .. N loop
         Result := Result * I;
      end loop;
      return Result;
   end Factorial;
   
   -- Read RNA string from input
   Line : Unbounded_String;
   S : RNA_String(1..100);
   Length : Natural;
   
   -- Count nucleotides
   A_Count, U_Count, C_Count, G_Count : Natural := 0;
   
begin
   -- Read the RNA sequence
   Get_Line(Line);
   Length := Length(Line);
   
   -- Copy to array and count nucleotides
   for I in 1 .. Length loop
      S(I) := To_String(Line)(I);
      case S(I) is
         when 'A' => A_Count := A_Count + 1;
         when 'U' => U_Count := U_Count + 1;
         when 'C' => C_Count := C_Count + 1;
         when 'G' => G_Count := G_Count + 1;
         when others => null;
      end case;
   end loop;
   
   -- For perfect matching, we need equal pairs of complementary bases
   -- A must pair with U, C must pair with G
   -- So we need to calculate the number of ways to match these pairs
   
   -- The result is the product of factorials of the counts of each complementary pair
   declare
      Result : Natural := 1;
   begin
      -- For A/U pairs: if we have n A's and n U's, there are n! ways to pair them
      -- But we only need to consider one of them since they're paired together
      -- Actually, we want to count the number of perfect matchings
      Result := Factorial(A_Count) * Factorial(C_Count);
      
      Put_Line(Result'Image);
   end;
   
end Perfect_Matchings;
```

Wait, let me reconsider this problem more carefully. 

Looking at it again, if we have an RNA sequence and want perfect matchings, we're looking for the number of ways to pair bases such that each base is in exactly one pair.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Perfect_Matchings is
   -- Function to compute factorial
   function Factorial(N : Natural) return Natural is
      Result : Natural := 1;
   begin
      if N = 0 or N = 1 then
         return 1;
      end if;
      
      for I in 2 .. N loop
         Result := Result * I;
      end loop;
      return Result;
   end Factorial;
   
   -- Read RNA string from input
   Line : String(1..100);
   Length : Natural;
   
   -- Count nucleotides
   A_Count, U_Count, C_Count, G_Count : Natural := 0;
   
begin
   -- Read the RNA sequence
   Get_Line(Line, Length);
   
   -- Count nucleotides
   for I in 1 .. Length loop
      case Line(I) is
         when 'A' => A_Count := A_Count + 1;
         when 'U' => U_Count := U_Count + 1;
         when 'C' => C_Count := C_Count + 1;
         when 'G' => G_Count := G_Count + 1;
         when others => null;
      end case;
   end loop;
   
   -- For perfect matchings in RNA secondary structure:
   -- A pairs with U, C pairs with G
   -- The number of perfect matchings is the product of factorials
   -- of the counts of each complementary pair
   
   -- Since A must pair with U and C must pair with G,
   -- we have n_A! ways to arrange A-U pairs and n_C! ways to arrange C-G pairs
   declare
      Result : Natural := 1;
   begin
      if A_Count /= U_Count or C_Count /= G_Count then
         Put_Line("0");  -- No perfect matching possible
         return;
      end if;
      
      Result := Factorial(A_Count) * Factorial(C_Count);
      
      Put_Line(Result'Image);
   end;
   
end Perfect_Matchings;
```

Actually, I think there's still an issue with my understanding. Let me correct it:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Perfect_Matchings is
   -- Function to compute factorial
   function Factorial(N : Natural) return Natural is
      Result : Natural := 1;
   begin
      if N = 0 or N = 1 then
         return 1;
      end if;
      
      for I in 2 .. N loop
         Result := Result * I;
      end loop;
      return Result;
   end Factorial;
   
   -- Read RNA string from input
   Line : String(1..100);
   Length : Natural;
   
   -- Count nucleotides
   A_Count, U_Count, C_Count, G_Count : Natural := 0;
   
begin
   -- Read the RNA sequence
   Get_Line(Line, Length);
   
   -- Count nucleotides
   for I in 1 .. Length loop
      case Line(I) is
         when 'A' => A_Count := A_Count + 1;
         when 'U' => U_Count := U_Count + 1;
         when 'C' => C_Count := C_Count + 1;
         when 'G' => G_Count := G_Count + 1;
         when others => null;
      end case;
   end loop;
   
   -- For perfect matchings in RNA secondary structure:
   -- A pairs with U, C pairs with G
   -- This is a standard problem: the number of perfect matchings is 
   -- n_A! * n_C! where n_A = n_U and n_C = n_G (for valid perfect matching)
   
   declare
      Result : Natural := 1;
   begin
      if A_Count /= U_Count or C_Count /= G_Count then
         Put_Line("0");  -- No perfect matching possible
         return;
      end if;
      
      -- For each pair of complementary bases, we calculate the number of 
      -- ways to match them. This is simply the factorial of the count.
      Result := Factorial(A_Count) * Factorial(C_Count);
      
      Put_Line(Result'Image);
   end;
   
end Perfect_Matchings;
```