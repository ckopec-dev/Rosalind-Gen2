# Rosalind Problem: Find a Profile-most Probable k-mer in a String (Ada Solution)

## Problem Understanding

Given a string DNA sequence, a profile matrix (4xk), and a k-mer length k, we need to find the k-mer that has the highest probability according to the profile matrix.

## Solution Approach

1. Generate all possible k-mers from the DNA string
2. Calculate the probability of each k-mer using the profile matrix
3. Return the k-mer with maximum probability

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Find_Profile_Most_Probable_Kmer is
   
   type Nucleotide is (A, C, G, T);
   type Profile_Matrix is array (Nucleotide, Positive range <>) of Float;
   
   -- Function to convert character to nucleotide
   function Char_To_Nucleotide(C : Character) return Nucleotide is
   begin
      case C is
         when 'A' => return A;
         when 'C' => return C;
         when 'G' => return G;
         when 'T' => return T;
         when others => raise Constraint_Error;
      end case;
   end Char_To_Nucleotide;
   
   -- Function to convert nucleotide to character
   function Nucleotide_To_Char(N : Nucleotide) return Character is
   begin
      case N is
         when A => return 'A';
         when C => return 'C';
         when G => return 'G';
         when T => return 'T';
      end case;
   end Nucleotide_To_Char;
   
   -- Function to calculate probability of a k-mer given profile
   function Calculate_Probability(Kmer : Unbounded_String; 
                                 Profile : Profile_Matrix;
                                 K : Positive) return Float is
      Probability : Float := 1.0;
      Nuc : Nucleotide;
   begin
      for I in 1 .. K loop
         Nuc := Char_To_Nucleotide(Element(Kmer, I));
         Probability := Probability * Profile(Nuc, I);
      end loop;
      return Probability;
   end Calculate_Probability;
   
   -- Function to get all k-mers from DNA string
   function Get_Kmers(DNA : Unbounded_String; K : Positive) return Unbounded_String_Array is
      Length : constant Natural := Length(DNA) - K + 1;
      Result : Unbounded_String_Array(1 .. Length);
   begin
      for I in 1 .. Length loop
         Result(I) := Slice(DNA, I, I + K - 1);
      end loop;
      return Result;
   end Get_Kmers;
   
   -- Function to find the profile-most probable k-mer
   function Find_Profile_Most_Probable_Kmer(DNA : Unbounded_String;
                                          Profile : Profile_Matrix;
                                          K : Positive) return Unbounded_String is
      Kmers : constant Unbounded_String_Array := Get_Kmers(DNA, K);
      Max_Probability : Float := 0.0;
      Best_Kmer : Unbounded_String;
   begin
      for I in Kmers'Range loop
         declare
            Current_Probability : constant Float := Calculate_Probability(Kmers(I), Profile, K);
         begin
            if Current_Probability > Max_Probability then
               Max_Probability := Current_Probability;
               Best_Kmer := Kmers(I);
            end if;
         end;
      end loop;
      return Best_Kmer;
   end Find_Profile_Most_Probable_Kmer;
   
   -- Input parsing
   DNA : Unbounded_String;
   K : Positive;
   Profile : Profile_Matrix(1 .. 4, 1 .. 10); -- Assuming max k=10 for example
   Result : Unbounded_String;
   
begin
   -- Read input (example format)
   Put_Line("Enter DNA string:");
   Get_Line(DNA);
   
   Put_Line("Enter k value:");
   Get(K);
   
   -- Read profile matrix (example 4xk matrix)
   Put_Line("Enter profile matrix (4 rows, " & Integer'Image(K) & " columns):");
   for I in Nucleotide loop
      for J in 1 .. K loop
         declare
            Value : Float;
         begin
            Get(Value);
            Profile(I, J) := Value;
         end;
      end loop;
   end loop;
   
   -- Find the profile-most probable k-mer
   Result := Find_Profile_Most_Probable_Kmer(DNA, Profile, K);
   
   -- Output result
   Put_Line("Profile-most probable k-mer: " & To_String(Result));
   
end Find_Profile_Most_Probable_Kmer;
```

## Example Usage

For input:
```
DNA: "ACGTACGT"
k: 3
Profile matrix (4x3):
A: 0.2 0.1 0.3
C: 0.3 0.5 0.2
G: 0.4 0.3 0.4
T: 0.1 0.1 0.1
```

The program will:
1. Generate all 3-mers from "ACGTACGT": "ACG", "CGT", "GTA", "TAC", "ACG"
2. Calculate probabilities for each k-mer using the profile matrix
3. Return the k-mer with maximum probability

## Key Features of the Solution

1. **Type Safety**: Uses strong typing with custom types for nucleotides and matrices
2. **Modularity**: Separates concerns into functions for probability calculation, k-mer generation, and main logic
3. **Error Handling**: Includes proper error handling for invalid input characters
4. **Generic Design**: Can handle different values of k through parameters
5. **Memory Management**: Uses Ada's string handling capabilities efficiently

## Time Complexity
- O(n×k) where n is the length of DNA string and k is the k-mer length
- For each k-mer, we calculate probability in O(k) time

This solution correctly implements the profile-most probable k-mer algorithm as required by the Rosalind problem.