# Rosalind Problem: Implement_GreedyMotifSearch_with_Pseudocounts in Ada

## Problem Understanding

This problem requires implementing the Greedy Motif Search algorithm with pseudocounts to find the best motif in a set of DNA sequences. The algorithm uses a greedy approach to find motifs that are conserved across sequences.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Implement_GreedyMotifSearch_with_Pseudocounts is
   
   type DNA_Nucleotide is ('A', 'C', 'G', 'T');
   type DNA_Sequence is array (Positive range <>) of DNA_Nucleotide;
   type DNA_Sequences is array (Positive range <>) of DNA_Sequence;
   
   package DNA_Vector is new Ada.Containers.Vectors (Positive, DNA_Sequence);
   package String_Vector is new Ada.Containers.Vectors (Positive, Unbounded_String);
   
   function Count_Nucleotides (Sequence : DNA_Sequence; Nucleotide : DNA_Nucleotide) return Natural is
   begin
      return (for sum I in Sequence'Range => 
                 (if Sequence(I) = Nucleotide then 1 else 0));
   end Count_Nucleotides;
   
   function Profile_Matrix (Sequences : DNA_Sequences; Start_Index : Positive; 
                           Length : Positive) return array (DNA_Nucleotide, Positive range 1..4) of Float is
      Profile : array (DNA_Nucleotide, Positive range 1..4) of Float := (others => (others => 0.0));
      Total : Natural := Sequences'Length;
   begin
      for I in Sequences'Range loop
         for J in Start_Index..Start_Index + Length - 1 loop
            case Sequences(I)(J) is
               when 'A' => Profile('A', J) := Profile('A', J) + 1.0;
               when 'C' => Profile('C', J) := Profile('C', J) + 1.0;
               when 'G' => Profile('G', J) := Profile('G', J) + 1.0;
               when 'T' => Profile('T', J) := Profile('T', J) + 1.0;
            end case;
         end loop;
      end loop;
      
      for J in 1..4 loop
         Profile('A', J) := Profile('A', J) / Float(Total);
         Profile('C', J) := Profile('C', J) / Float(Total);
         Profile('G', J) := Profile('G', J) / Float(Total);
         Profile('T', J) := Profile('T', J) / Float(Total);
      end loop;
      
      return Profile;
   end Profile_Matrix;
   
   function Profile_Score (Sequence : DNA_Sequence; Profile : array (DNA_Nucleotide, Positive range 1..4) of Float) 
                          return Float is
      Score : Float := 0.0;
   begin
      for I in Sequence'Range loop
         case Sequence(I) is
            when 'A' => Score := Score + Profile('A', I);
            when 'C' => Score := Score + Profile('C', I);
            when 'G' => Score := Score + Profile('G', I);
            when 'T' => Score := Score + Profile('T', I);
         end case;
      end loop;
      return Score;
   end Profile_Score;
   
   function Most_Probable_Kmer (Sequence : DNA_Sequence; Profile : array (DNA_Nucleotide, Positive range 1..4) of Float; 
                               K : Positive) return DNA_Sequence is
      Max_Score : Float := -1000000.0;
      Best_Kmer : DNA_Sequence (1..K);
   begin
      for I in 1..Sequence'Length - K + 1 loop
         declare
            Kmer : DNA_Sequence (1..K);
            Score : Float := 0.0;
         begin
            for J in 1..K loop
               Kmer(J) := Sequence(I + J - 1);
            end loop;
            
            for J in 1..K loop
               case Kmer(J) is
                  when 'A' => Score := Score + Profile('A', J);
                  when 'C' => Score := Score + Profile('C', J);
                  when 'G' => Score := Score + Profile('G', J);
                  when 'T' => Score := Score + Profile('T', J);
               end case;
            end loop;
            
            if Score > Max_Score then
               Max_Score := Score;
               Best_Kmer := Kmer;
            end if;
         end;
      end loop;
      
      return Best_Kmer;
   end Most_Probable_Kmer;
   
   function Consensus_String (Motifs : DNA_Sequences) return DNA_Sequence is
      Length : constant Positive := Motifs(1)'Length;
      Consensus : DNA_Sequence (1..Length);
   begin
      for I in 1..Length loop
         declare
            Count_A, Count_C, Count_G, Count_T : Natural := 0;
         begin
            for J in Motifs'Range loop
               case Motifs(J)(I) is
                  when 'A' => Count_A := Count_A + 1;
                  when 'C' => Count_C := Count_C + 1;
                  when 'G' => Count_G := Count_G + 1;
                  when 'T' => Count_T := Count_T + 1;
               end case;
            end loop;
            
            if Count_A >= Count_C and Count_A >= Count_G and Count_A >= Count_T then
               Consensus(I) := 'A';
            elsif Count_C >= Count_G and Count_C >= Count_T then
               Consensus(I) := 'C';
            elsif Count_G >= Count_T then
               Consensus(I) := 'G';
            else
               Consensus(I) := 'T';
            end if;
         end;
      end loop;
      
      return Consensus;
   end Consensus_String;
   
   function Score (Motifs : DNA_Sequences) return Natural is
      Consensus : DNA_Sequence := Consensus_String(Motifs);
      Score : Natural := 0;
   begin
      for I in Motifs'Range loop
         for J in Motifs(I)'Range loop
            if Motifs(I)(J) /= Consensus(J) then
               Score := Score + 1;
            end if;
         end loop;
      end loop;
      return Score;
   end Score;
   
   function Greedy_Motif_Search_with_Pseudocounts (Sequences : DNA_Sequences; K : Positive) return DNA_Sequences is
      Best_Motifs : DNA_Sequences (Sequences'Range);
      Best_Score : Natural := Integer'Last;
   begin
      for I in 1..Sequences(1)'Length - K + 1 loop
         declare
            Motifs : DNA_Sequences (Sequences'Range);
            Profile : array (DNA_Nucleotide, Positive range 1..4) of Float;
         begin
            -- Initialize first motif
            for J in 1..K loop
               Motifs(1)(J) := Sequences(1)(I + J - 1);
            end loop;
            
            -- Greedy search
            for L in 2..Sequences'Length loop
               -- Build profile with pseudocounts
               Profile := Profile_Matrix(Motifs(1..L-1), 1, K);
               
               -- Add pseudocounts
               for N in DNA_Nucleotide loop
                  for J in 1..K loop
                     Profile(N, J) := (Profile(N, J) * Float(L-1) + 1.0) / Float(L-1 + 4);
                  end loop;
               end loop;
               
               -- Find most probable k-mer
               Motifs(L) := Most_Probable_Kmer(Sequences(L), Profile, K);
            end loop;
            
            -- Check if this is better
            if Score(Motifs) < Best_Score then
               Best_Score := Score(Motifs);
               Best_Motifs := Motifs;
            end if;
         end;
      end loop;
      
      return Best_Motifs;
   end Greedy_Motif_Search_with_Pseudocounts;
   
   -- Example usage
   Sequences : DNA_Sequences (1..5) := (
      (DNA_Nucleotide'('T', 'G', 'C', 'G', 'G', 'T', 'G', 'T', 'T', 'G')),
      (DNA_Nucleotide'('T', 'A', 'G', 'C', 'G', 'A', 'G', 'T', 'T', 'A')),
      (DNA_Nucleotide'('T', 'G', 'C', 'G', 'G', 'T', 'G', 'T', 'T', 'G')),
      (DNA_Nucleotide'('T', 'A', 'G', 'C', 'G', 'A', 'G', 'T', 'T', 'A')),
      (DNA_Nucleotide'('T', 'G', 'C', 'G', 'G', 'T', 'G', 'T', 'T', 'G'))
   );
   
   K : constant Positive := 3;
   Best_Motifs : DNA_Sequences;
   
begin
   Best_Motifs := Greedy_Motif_Search_with_Pseudocounts(Sequences, K);
   
   Put_Line("Best Motifs:");
   for I in Best_Motifs'Range loop
      for J in Best_Motifs(I)'Range loop
         Put(Character'Val(Character'Pos(DNA_Nucleotide'Pos(Best_Motifs(I)(J))) + Character'Pos('A')));
      end loop;
      New_Line;
   end loop;
   
   Put_Line("Score: " & Integer'Image(Score(Best_Motifs)));
end Implement_GreedyMotifSearch_with_Pseudocounts;
```

## Key Features of the Implementation

1. **Data Structures**: 
   - Uses arrays to represent DNA sequences with nucleotides A, C, G, T
   - Implements proper type definitions for DNA sequences and motifs

2. **Pseudocounts Implementation**:
   - Adds pseudocounts to the profile matrix to avoid zero probabilities
   - Uses the formula: (count * (n-1) + 1) / (n-1 + 4) where n is the number of sequences

3. **Core Functions**:
   - `Count_Nucleotides`: Counts occurrences of a specific nucleotide
   - `Profile_Matrix`: Builds a profile matrix from motifs
   - `Most_Probable_Kmer`: Finds the most probable k-mer given a profile
   - `Consensus_String`: Computes consensus string from motifs
   - `Score`: Calculates the score of a set of motifs
   - `Greedy_Motif_Search_with_Pseudocounts`: Main algorithm implementation

4. **Algorithm Steps**:
   - Try all possible k-mers in the first sequence as the initial motif
   - For each subsequent sequence, find the most probable k-mer using the current profile
   - Add pseudocounts to prevent zero probabilities
   - Keep track of the best scoring set of motifs

This implementation correctly handles the pseudocounts requirement that prevents zero probabilities in the profile matrix, which is crucial for the greedy algorithm to work properly.

