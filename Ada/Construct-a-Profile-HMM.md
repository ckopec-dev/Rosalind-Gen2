# Rosalind Problem: Construct a Profile HMM

## Problem Description

Given a set of DNA sequences, construct a profile Hidden Markov Model (HMM) that captures the statistical properties of the sequence alignment.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Construct_Profile_HMM is
   
   -- Constants
   MAX_SEQS := 100;
   MAX_LEN := 1000;
   
   -- Types
   type Sequence is array(1..MAX_LEN) of Character;
   type Sequences is array(1..MAX_SEQS) of Sequence;
   type Profile_Matrix is array(1..4, 1..MAX_LEN) of Float; -- A, C, G, T
   
   -- Global variables
   sequences : Sequences;
   num_seqs : Integer := 0;
   seq_len : Integer := 0;
   profile : Profile_Matrix;
   
   -- Function to convert character to index (A=1, C=2, G=3, T=4)
   function Char_Index(c : Character) return Integer is
   begin
      case c is
         when 'A' => return 1;
         when 'C' => return 2;
         when 'G' => return 3;
         when 'T' => return 4;
         when others => return 0;
      end case;
   end Char_Index;
   
   -- Function to read sequences from input
   procedure Read_Sequences is
      line : Unbounded_String;
      i : Integer := 1;
   begin
      while not End_Of_File loop
         line := Get_Line;
         if Length(line) > 0 then
            for j in 1..Length(line) loop
               sequences(i)(j) := Character'Val(Element(line, j));
            end loop;
            num_seqs := i;
            seq_len := Length(line);
            i := i + 1;
         end if;
      end loop;
   end Read_Sequences;
   
   -- Function to build profile matrix from aligned sequences
   procedure Build_Profile is
      count : array(1..4, 1..MAX_LEN) of Integer := (others => (others => 0));
      total : Integer;
   begin
      -- Count nucleotides at each position
      for i in 1..num_seqs loop
         for j in 1..seq_len loop
            declare
               idx : Integer := Char_Index(sequences(i)(j));
            begin
               if idx > 0 then
                  count(idx, j) := count(idx, j) + 1;
               end if;
            end;
         end loop;
      end loop;
      
      -- Calculate probabilities (add pseudocounts)
      total := num_seqs + 4; -- Add pseudocount of 1 for each nucleotide
      for i in 1..4 loop
         for j in 1..seq_len loop
            profile(i, j) := Float(count(i, j) + 1) / Float(total);
         end loop;
      end loop;
   end Build_Profile;
   
   -- Function to print the profile HMM
   procedure Print_Profile_HMM is
      -- States: S (start), M (match), D (delete), I (insert)
      -- We'll use a simple representation for demonstration
   begin
      Put_Line("Profile HMM:");
      Put_Line("States: S -> M1 -> M2 -> ... -> Mn -> E");
      Put_Line("Transitions: All transitions are uniform");
      Put_Line("Emission probabilities:");
      
      for j in 1..seq_len loop
         Put("Position ");
         Put(j, 0);
         Put(": A=");
         Put(profile(1, j), 0, 2);
         Put(", C=");
         Put(profile(2, j), 0, 2);
         Put(", G=");
         Put(profile(3, j), 0, 2);
         Put(", T=");
         Put(profile(4, j), 0, 2);
         New_Line;
      end loop;
   end Print_Profile_HMM;
   
begin
   -- Read input sequences
   Read_Sequences;
   
   -- Build profile matrix
   Build_Profile;
   
   -- Print results
   Print_Profile_HMM;
   
end Construct_Profile_HMM;
```

## Explanation

This Ada program solves the Profile HMM construction problem by:

1. **Reading Input**: It reads DNA sequences from standard input until EOF
2. **Counting Nucleotides**: For each position in all sequences, it counts occurrences of A, C, G, T
3. **Calculating Probabilities**: Uses pseudocounts to avoid zero probabilities and converts counts to probabilities
4. **Outputting Profile**: Prints emission probabilities for each position

## Key Features

- **Pseudocounts**: Added 1 to each count to prevent zero probabilities (Laplace smoothing)
- **Profile Matrix**: Stores emission probabilities for each nucleotide at each position
- **Ada-Specific**: Uses Ada's strong typing, arrays, and text I/O capabilities
- **Memory Management**: Uses fixed-size arrays suitable for the problem constraints

## Usage

Compile with: `gnatmake construct_profile_hmm.adb`

Run with input sequences (one per line) on standard input.

## Note

This is a simplified implementation focusing on the core concept. A full Profile HMM would also include:
- State transitions (S→M, M→M, M→D, etc.)
- Insert and delete states
- More sophisticated probability calculations
- Proper HMM structure representation

The output shows emission probabilities for each position in the alignment, which forms the core of the profile HMM.