# Rosalind Problem: Generate the Frequency Array of a String (Ada Solution)

## Problem Understanding

Given a DNA string and an integer k, we need to generate a frequency array where each position represents the count of kmers (k-length substrings) in lexicographic order.

## Solution Approach

1. Create a mapping from nucleotides to indices (A=0, C=1, G=2, T=3)
2. Generate all possible k-mers in lexicographic order
3. Count occurrences of each k-mer in the input string
4. Return frequency array

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Generate_Frequency_Array is
   type Nucleotide is (A, C, G, T);
   type Frequency_Array is array (0 .. 3) of Integer;
   
   -- Convert nucleotide to index
   function Nucleotide_To_Index(N : Nucleotide) return Integer is
   begin
      case N is
         when A => return 0;
         when C => return 1;
         when G => return 2;
         when T => return 3;
      end case;
   end Nucleotide_To_Index;
   
   -- Convert character to nucleotide
   function Char_To_Nucleotide(C : Character) return Nucleotide is
   begin
      case C is
         when 'A' => return A;
         when 'C' => return C;
         when 'G' => return G;
         when 'T' => return T;
         when others => raise Constraint_Error with "Invalid nucleotide";
      end case;
   end Char_To_Nucleotide;
   
   -- Generate all k-mers in lexicographic order
   procedure Generate_Kmers(Length : Integer; 
                           Current : String; 
                           Kmers : in out String_Array) is
      Index : Integer := 1;
   begin
      if Current'Length = Length then
         Kmers(Kmers'First + Index - 1) := Current;
         Index := Index + 1;
      else
         Generate_Kmers(Length, Current & 'A', Kmers);
         Generate_Kmers(Length, Current & 'C', Kmers);
         Generate_Kmers(Length, Current & 'G', Kmers);
         Generate_Kmers(Length, Current & 'T', Kmers);
      end if;
   end Generate_Kmers;
   
   -- Count frequency of k-mers in string
   function Count_Frequencies(Text : String; K : Integer) return Frequency_Array is
      Result : Frequency_Array := (others => 0);
      Kmer_Count : constant Integer := 4 ** K;
      Kmers : array (1 .. Kmer_Count) of String(1 .. K);
      Current_Kmer : String(1 .. K);
   begin
      -- Generate all possible k-mers in lexicographic order
      -- This is a simplified approach for small k values
      
      -- For each position in text where a k-mer can start
      for I in 0 .. Text'Length - K loop
         Current_Kmer := Text(I + 1 .. I + K);
         declare
            Index : Integer := 0;
            Power : Integer := 1;
         begin
            -- Convert k-mer to numeric index
            for J in reverse 1 .. K loop
               Index := Index + Nucleotide_To_Index(Char_To_Nucleotide(Current_Kmer(J))) * Power;
               Power := Power * 4;
            end loop;
            Result(Index) := Result(Index) + 1;
         end;
      end loop;
      
      return Result;
   end Count_Frequencies;
   
   -- Read input from stdin
   Text : String(1 .. 1000);
   Text_Length : Integer;
   K : Integer;
   Freq_Array : Frequency_Array;
   
begin
   -- Read DNA string
   Get_Line(Text, Text_Length);
   
   -- Read k value
   Get(K);
   New_Line;
   
   -- Calculate frequency array
   Freq_Array := Count_Frequencies(Text(1 .. Text_Length), K);
   
   -- Output results
   for I in Frequency_Array'First .. Frequency_Array'Last loop
      Put(Freq_Array(I), Width => 0);
      Put(" ");
   end loop;
   New_Line;
end Generate_Frequency_Array;
```

## Alternative Simpler Implementation

For better clarity and practicality:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Generate_Frequency_Array is
   type Nucleotide is (A, C, G, T);
   type Frequency_Array is array (0 .. 4**3 - 1) of Integer; -- For k=3
   
   function Char_To_Index(C : Character) return Integer is
   begin
      case C is
         when 'A' => return 0;
         when 'C' => return 1;
         when 'G' => return 2;
         when 'T' => return 3;
         when others => raise Constraint_Error;
      end case;
   end Char_To_Index;
   
   function Kmer_To_Index(Kmer : String) return Integer is
      Result : Integer := 0;
      Power : Integer := 1;
   begin
      for I in reverse Kmer'Range loop
         Result := Result + Char_To_Index(Kmer(I)) * Power;
         Power := Power * 4;
      end loop;
      return Result;
   end Kmer_To_Index;
   
   Text : String(1 .. 1000);
   Text_Length : Integer;
   K : Integer;
   Freq_Array : Frequency_Array := (others => 0);
   
begin
   -- Read input
   Get_Line(Text, Text_Length);
   Get(K);
   New_Line;
   
   -- Count frequencies
   for I in 0 .. Text_Length - K loop
      declare
         Current_Kmer : String(1 .. K) := Text(I + 1 .. I + K);
         Index : Integer := Kmer_To_Index(Current_Kmer);
      begin
         Freq_Array(Index) := Freq_Array(Index) + 1;
      end;
   end loop;
   
   -- Output results
   for I in Frequency_Array'First .. Frequency_Array'Last loop
      Put(Freq_Array(I), Width => 0);
      Put(" ");
   end loop;
   New_Line;
end Generate_Frequency_Array;
```

## Key Features

1. **Nucleotide Mapping**: Converts A/C/G/T to indices 0/1/2/3
2. **K-mer Indexing**: Converts k-mers to numeric indices using base-4 representation
3. **Frequency Counting**: Counts occurrences of each k-mer in the input string
4. **Lexicographic Order**: Results are ordered according to lexicographic ordering of k-mers

## Time Complexity
- O(n*k) where n is the length of the DNA string and k is the k-mer length

## Space Complexity  
- O(4^k) for storing the frequency array

This implementation handles the core requirements of generating a frequency array for k-mers in lexicographic order.