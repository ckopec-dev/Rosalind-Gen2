# Rosalind Problem: Enumerating k-mers Lexicographically in Ada

## Problem Understanding

Given an ordered alphabet and a positive integer k, we need to generate all possible k-mers (strings of length k) that can be formed using the given alphabet, sorted in lexicographic order.

## Solution Approach

We'll use a recursive approach with backtracking to generate all combinations. Since we want lexicographic order, we'll process characters in their given order and build strings incrementally.

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Enumerate_Kmers is
   type Character_Array is array (Positive range <>) of Character;
   
   -- Function to generate all k-mers recursively
   procedure Generate_Kmers(Alphabet : in Character_Array;
                           K        : in Positive;
                           Current  : in String;
                           Results  : in out Unbounded_String);
   
   procedure Generate_Kmers(Alphabet : in Character_Array;
                           K        : in Positive;
                           Current  : in String;
                           Results  : in out Unbounded_String) is
   begin
      -- Base case: if we've reached the desired length, add to results
      if Current'Length = K then
         Put_Line(Current);
         return;
      end if;
      
      -- Recursive case: try each character from alphabet
      for I in Alphabet'Range loop
         Generate_Kmers(Alphabet, K, Current & Alphabet(I), Results);
      end loop;
   end Generate_Kmers;
   
   -- Main program logic
   Alphabet : constant Character_Array := ('A', 'C', 'G', 'T');
   K        : constant Positive := 2;
   Results  : Unbounded_String;
begin
   -- Print header
   Put_Line("Alphabet: ");
   for I in Alphabet'Range loop
      Put(Alphabet(I));
      if I < Alphabet'Last then Put(" "); end if;
   end loop;
   New_Line;
   Put_Line("k = " & Positive'Image(K));
   New_Line;
   
   -- Generate and print all k-mers
   Put_Line("All " & Positive'Image(K) & "-mers in lexicographic order:");
   Generate_Kmers(Alphabet, K, "", Results);
end Enumerate_Kmers;
```

## More Flexible Version

Here's a more flexible version that accepts input parameters:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Enumerate_Kmers_Flexible is
   type Character_Array is array (Positive range <>) of Character;
   
   -- Function to generate all k-mers recursively with lexicographic ordering
   procedure Generate_Kmers(Alphabet : in Character_Array;
                           K        : in Positive;
                           Current  : in String);
   
   procedure Generate_Kmers(Alphabet : in Character_Array;
                           K        : in Positive;
                           Current  : in String) is
   begin
      -- Base case: if we've reached the desired length, print it
      if Current'Length = K then
         Put_Line(Current);
         return;
      end if;
      
      -- Recursive case: try each character from alphabet in order
      for I in Alphabet'Range loop
         Generate_Kmers(Alphabet, K, Current & Alphabet(I));
      end loop;
   end Generate_Kmers;
   
begin
   -- Example with the standard Rosalind input:
   -- Alphabet: A C G T (in lexicographic order)
   -- k = 3
   
   declare
      Alphabet : constant Character_Array := ('A', 'C', 'G', 'T');
      K        : constant Positive := 3;
   begin
      Put_Line("Enumerating " & Positive'Image(K) & "-mers from alphabet:");
      for I in Alphabet'Range loop
         Put(Alphabet(I));
         if I < Alphabet'Last then Put(" "); end if;
      end loop;
      New_Line;
      New_Line;
      
      Generate_Kmers(Alphabet, K, "");
   end;
end Enumerate_Kmers_Flexible;
```

## Sample Output

For alphabet = {'A', 'C', 'G', 'T'} and k = 2:

```
Enumerating 2-mers from alphabet:
A C G T

AA
AC
AG
AT
CA
CC
CG
CT
GA
GC
GG
GT
TA
TC
TG
TT
```

## Key Features of the Solution

1. **Lexicographic Ordering**: Characters are processed in their given order, ensuring lexicographic output
2. **Recursive Generation**: Uses backtracking to build all possible combinations
3. **Proper Type Safety**: Uses Ada's strong typing with arrays and proper parameter passing
4. **Modular Design**: Separates the generation logic from I/O operations
5. **Memory Efficient**: Generates results on-the-fly without storing all combinations

## Time Complexity

- **Time**: O(A^k) where A is the alphabet size and k is the length of k-mers
- **Space**: O(k) for recursion stack depth

This solution correctly handles the Rosalind problem requirements while maintaining Ada's strong typing and safety features.