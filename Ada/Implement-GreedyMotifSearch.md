# Rosalind Problem: Implement_GreedyMotifSearch in Ada

## Problem Understanding

The Greedy Motif Search algorithm is used to find the best motif (consensus pattern) across multiple DNA sequences. The algorithm works by:
1. Starting with the first k-mer from the first string
2. Building a profile matrix from current motifs
3. Finding the most probable k-mer in each subsequent string
4. Continuing until all strings are processed

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Implement_GreedyMotifSearch is
   
   -- Type definitions
   type DNA_String is array (Positive range <>) of Character;
   type DNA_Vector is array (Positive range <>) of DNA_String;
   
   package String_Vectors is new Ada.Containers.Vectors (Positive, Unbounded_String);
   use String_Vectors;
   
   -- Constants
   k : constant Positive := 3;  -- Motif length
   t : constant Positive := 5;  -- Number of DNA strings
   
   -- Function to calculate Hamming distance between two DNA strings
   function Hamming_Distance(s1, s2 : DNA_String) return Natural is
      distance : Natural := 0;
   begin
      for i in s1'First .. s1'Last loop
         if s1(i) /= s2(i) then
            distance := distance + 1;
         end if;
      end loop;
      return distance;
   end Hamming_Distance;
   
   -- Function to calculate score of a motif matrix
   function Score_Motifs(motifs : DNA_Vector) return Natural is
      score : Natural := 0;
      consensus : DNA_String(1 .. k);
   begin
      -- Build consensus string
      for i in 1 .. k loop
         declare
            count_A, count_C, count_G, count_T : Natural := 0;
            max_count : Natural := 0;
            max_char : Character;
         begin
            for j in motifs'First .. motifs'Last loop
               case motifs(j)(i) is
                  when 'A' => count_A := count_A + 1;
                  when 'C' => count_C := count_C + 1;
                  when 'G' => count_G := count_G + 1;
                  when 'T' => count_T := count_T + 1;
               end case;
            end loop;
            
            -- Find most frequent nucleotide
            max_count := count_A;
            max_char := 'A';
            
            if count_C > max_count then
               max_count := count_C;
               max_char := 'C';
            end if;
            
            if count_G > max_count then
               max_count := count_G;
               max_char := 'G';
            end if;
            
            if count_T > max_count then
               max_count := count_T;
               max_char := 'T';
            end if;
            
            consensus(i) := max_char;
         end;
      end loop;
      
      -- Calculate total score
      for i in motifs'First .. motifs'Last loop
         score := score + Hamming_Distance(motifs(i), consensus);
      end loop;
      
      return score;
   end Score_Motifs;
   
   -- Function to get all k-mers from a DNA string
   function Get_Kmers(dna : Unbounded_String; k_val : Positive) return DNA_Vector is
      kmers : DNA_Vector(1 .. dna.Length - k_val + 1);
      i : Positive;
   begin
      i := 1;
      for j in 1 .. kmers'Length loop
         declare
            temp : Unbounded_String;
         begin
            temp := Slice(dna, i, i + k_val - 1);
            kmers(j) := DNA_String'(1 .. k_val => Character'Value(To_String(temp)));
            i := i + 1;
         end;
      end loop;
      return kmers;
   end Get_Kmers;
   
   -- Function to get profile matrix
   function Get_Profile(motifs : DNA_Vector; k_val : Positive) return array (1 .. k_val, 'A' .. 'T') of Float is
      profile : array (1 .. k_val, 'A' .. 'T') of Float := (others => (others => 0.0));
      total : Natural := motifs'Length;
   begin
      for i in 1 .. k_val loop
         declare
            count_A, count_C, count_G, count_T : Natural := 0;
         begin
            for j in motifs'First .. motifs'Last loop
               case motifs(j)(i) is
                  when 'A' => count_A := count_A + 1;
                  when 'C' => count_C := count_C + 1;
                  when 'G' => count_G := count_G + 1;
                  when 'T' => count_T := count_T + 1;
               end case;
            end loop;
            
            profile(i, 'A') := Float(count_A) / Float(total);
            profile(i, 'C') := Float(count_C) / Float(total);
            profile(i, 'G') := Float(count_G) / Float(total);
            profile(i, 'T') := Float(count_T) / Float(total);
         end;
      end loop;
      
      return profile;
   end Get_Profile;
   
   -- Function to find best k-mer in a DNA string based on profile
   function Get_Best_Kmer(dna : Unbounded_String; profile : array (1 .. k, 'A' .. 'T') of Float; k_val : Positive) return DNA_String is
      best_score : Float := -1.0;
      best_kmer : DNA_String(1 .. k);
   begin
      for i in 1 .. dna.Length - k_val + 1 loop
         declare
            current_kmer : DNA_String(1 .. k_val);
            score : Float := 1.0;
         begin
            -- Extract k-mer
            for j in 1 .. k_val loop
               current_kmer(j) := Character'Value(To_String(Slice(dna, i + j - 1, i + j - 1)));
            end loop;
            
            -- Calculate probability score
            for j in 1 .. k_val loop
               score := score * profile(j, current_kmer(j));
            end loop;
            
            if score > best_score then
               best_score := score;
               best_kmer := current_kmer;
            end if;
         end;
      end loop;
      
      return best_kmer;
   end Get_Best_Kmer;
   
   -- Main Greedy Motif Search function
   function Greedy_Motif_Search(dna_strings : array (1 .. t) of Unbounded_String; k_val : Positive) return DNA_Vector is
      best_motifs : DNA_Vector(1 .. t);
      current_motifs : DNA_Vector(1 .. t);
      best_score : Natural := Natural'Last;
   begin
      -- Try all possible k-mers from first string as starting point
      declare
         kmers_from_first : DNA_Vector;
      begin
         kmers_from_first := Get_Kmers(dna_strings(1), k_val);
         
         for i in kmers_from_first'First .. kmers_from_first'Last loop
            -- Initialize current motifs with first k-mer from first string
            current_motifs(1) := kmers_from_first(i);
            
            -- Build motifs greedily
            for j in 2 .. t loop
               declare
                  profile : array (1 .. k_val, 'A' .. 'T') of Float;
                  best_kmer : DNA_String(1 .. k_val);
               begin
                  profile := Get_Profile(current_motifs(1 .. j-1), k_val);
                  best_kmer := Get_Best_Kmer(dna_strings(j), profile, k_val);
                  current_motifs(j) := best_kmer;
               end;
            end loop;
            
            -- Check if this is better than current best
            if Score_Motifs(current_motifs(1 .. t)) < best_score then
               best_score := Score_Motifs(current_motifs(1 .. t));
               best_motifs := current_motifs;
            end if;
         end loop;
      end;
      
      return best_motifs;
   end Greedy_Motif_Search;
   
   -- Example DNA strings (from Rosalind problem)
   dna_sequences : array (1 .. 5) of Unbounded_String := (
      To_Unbounded_String("GGCGTTCAGGCA"),
      To_Unbounded_String("AAGAATCAGTCA"),
      To_Unbounded_String("CAAGGAGTTCGC"),
      To_Unbounded_String("CACGTCAATCAC"),
      To_Unbounded_String("CAATAATATCAC")
   );
   
   -- Result
   result_motifs : DNA_Vector;
   
begin
   Put_Line("Running Greedy Motif Search...");
   result_motifs := Greedy_Motif_Search(dna_sequences, k);
   
   Put_Line("Best motifs found:");
   for i in result_motifs'First .. result_motifs'Last loop
      Put_Line("Motif " & Integer'Image(i) & ": " & 
               String'(result_motifs(i)));
   end loop;
   
   Put_Line("Score: " & Natural'Image(Score_Motifs(result_motifs)));
end Implement_GreedyMotifSearch;
```

## Explanation of Key Components

1. **Data Structures**: 
   - `DNA_String` for individual DNA sequences
   - `DNA_Vector` for collections of DNA strings
   - Profile matrix represented as a 2D array with floating-point values

2. **Core Functions**:
   - `Hamming_Distance`: Calculates distance between two DNA strings
   - `Score_Motifs`: Computes the total score of a motif matrix
   - `Get_Kmers`: Extracts all k-mers from a DNA string
   - `Get_Profile`: Builds a profile matrix from motifs
   - `Get_Best_Kmer`: Finds the most probable k-mer based on profile
   - `Greedy_Motif_Search`: Implements the main algorithm

3. **Algorithm Steps**:
   - Try all possible starting k-mers from the first string
   - For each starting k-mer, build motifs greedily by finding best k-mer in each subsequent string
   - Keep track of the best scoring motif set found

This implementation handles the core requirements of the Greedy Motif Search algorithm while using Ada's strong typing and structured programming features.