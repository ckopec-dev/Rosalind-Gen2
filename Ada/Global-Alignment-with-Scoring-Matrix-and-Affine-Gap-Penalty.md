# Rosalind Problem: Global Alignment with Scoring Matrix and Affine Gap Penalty

## Problem Understanding

We need to implement global sequence alignment using a scoring matrix with affine gap penalties. This is a classic dynamic programming problem that extends the basic Needleman-Wunsch algorithm.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Global_Alignment_with_Scoring_Matrix_and_Affine_Gap_Penalty is
   
   type Matrix is array (Positive range <>, Positive range <>) of Integer;
   
   -- Function to read scoring matrix from input
   function Read_Scoring_Matrix(Num_Amino_Acids : Positive) return Matrix is
      M : Matrix(1..Num_Amino_Acids, 1..Num_Amino_Acids);
   begin
      for i in 1..Num_Amino_Acids loop
         for j in 1..Num_Amino_Acids loop
            Get(M(i,j));
         end loop;
      end loop;
      return M;
   end Read_Scoring_Matrix;
   
   -- Function to get amino acid index (assuming A=1, C=2, D=3, etc.)
   function Get_Index(Amino : Character) return Positive is
   begin
      case Amino is
         when 'A' => return 1;
         when 'C' => return 2;
         when 'D' => return 3;
         when 'E' => return 4;
         when 'F' => return 5;
         when 'G' => return 6;
         when 'H' => return 7;
         when 'I' => return 8;
         when 'K' => return 9;
         when 'L' => return 10;
         when 'M' => return 11;
         when 'N' => return 12;
         when 'P' => return 13;
         when 'Q' => return 14;
         when 'R' => return 15;
         when 'S' => return 16;
         when 'T' => return 17;
         when 'V' => return 18;
         when 'W' => return 19;
         when 'Y' => return 20;
         when others => return 1;
      end case;
   end Get_Index;
   
   -- Function to compute global alignment with affine gap penalty
   procedure Compute_Global_Alignment(
      Seq1, Seq2 : Unbounded_String;
      Score_Matrix : Matrix;
      Gap_Open : Integer;
      Gap_Extend : Integer;
      Score : out Integer;
      Alignment1, Alignment2 : out Unbounded_String) is
      
      N : constant Positive := Length(Seq1);
      M : constant Positive := Length(Seq2);
      
      -- DP matrices for affine gap penalty
      F : Matrix(0..N, 0..M);  -- Score matrix for alignment
      E : Matrix(0..N, 0..M);  -- Gap extension matrix (horizontal)
      H : Matrix(0..N, 0..M);  -- Gap extension matrix (vertical)
      
      -- Traceback matrices to reconstruct alignment
      Trace : array(0..N, 0..M) of Character;
      
      A1, A2 : Unbounded_String;
   begin
      -- Initialize matrices
      F := (others => (others => 0));
      E := (others => (others => 0));
      H := (others => (others => 0));
      Trace := (others => (others => '0'));
      
      -- Initialize first row and column
      for i in 1..N loop
         F(i,0) := Gap_Open + (i-1) * Gap_Extend;
         E(i,0) := Gap_Open + (i-1) * Gap_Extend;
         H(i,0) := Integer'Last;  -- Invalid
         Trace(i,0) := 'H';
      end loop;
      
      for j in 1..M loop
         F(0,j) := Gap_Open + (j-1) * Gap_Extend;
         E(0,j) := Integer'Last;  -- Invalid
         H(0,j) := Gap_Open + (j-1) * Gap_Extend;
         Trace(0,j) := 'V';
      end loop;
      
      -- Fill the matrices using dynamic programming
      for i in 1..N loop
         for j in 1..M loop
            declare
               Match_Score : constant Integer := Score_Matrix(Get_Index(Element(Seq1,i)), Get_Index(Element(Seq2,j)));
               E_Score : constant Integer := F(i-1,j) + Gap_Open;
               H_Score : constant Integer := F(i,j-1) + Gap_Open;
               F_Score : constant Integer := F(i-1,j-1) + Match_Score;
               
               -- Check if we can extend a gap
               E_Ext : constant Integer := E(i-1,j) + Gap_Extend;
               H_Ext : constant Integer := H(i,j-1) + Gap_Extend;
            begin
               -- Compute E(i,j) - horizontal gap
               E(i,j) := Integer'Max(E_Score, E_Ext);
               
               -- Compute H(i,j) - vertical gap  
               H(i,j) := Integer'Max(H_Score, H_Ext);
               
               -- Compute F(i,j) - match/mismatch or gap opening
               F(i,j) := Integer'Max(F_Score, Integer'Max(E(i,j), H(i,j)));
               
               -- Determine traceback direction
               if F(i,j) = F_Score then
                  Trace(i,j) := 'M';  -- Match/mismatch
               elsif F(i,j) = E(i,j) then
                  Trace(i,j) := 'E';  -- Extension or opening of horizontal gap
               else
                  Trace(i,j) := 'H';  -- Extension or opening of vertical gap
               end if;
            end;
         end loop;
      end loop;
      
      Score := F(N,M);
      
      -- Backtrack to construct alignment
      A1 := Null_Unbounded_String;
      A2 := Null_Unbounded_String;
      declare
         i : Positive := N;
         j : Positive := M;
         Current_Direction : Character := Trace(i,j);
      begin
         while i > 0 or j > 0 loop
            case Current_Direction is
               when 'M' =>
                  A1 := A1 & Element(Seq1,i);
                  A2 := A2 & Element(Seq2,j);
                  i := i - 1;
                  j := j - 1;
                  Current_Direction := Trace(i,j);
                  
               when 'E' =>
                  -- Horizontal gap
                  A1 := A1 & '-';
                  A2 := A2 & Element(Seq2,j);
                  j := j - 1;
                  Current_Direction := Trace(i,j);
                  
               when 'H' =>
                  -- Vertical gap  
                  A1 := A1 & Element(Seq1,i);
                  A2 := A2 & '-';
                  i := i - 1;
                  Current_Direction := Trace(i,j);
                  
               when others =>
                  raise Program_Error;
            end case;
         end loop;
         
         Alignment1 := Reverse_String(A1);
         Alignment2 := Reverse_String(A2);
      end;
   end Compute_Global_Alignment;

   -- Input reading and processing
   Seq1 : Unbounded_String;
   Seq2 : Unbounded_String;
   Score_Matrix : Matrix(1..20, 1..20);
   Gap_Open : Integer;
   Gap_Extend : Integer;
   Score : Integer;
   Alignment1, Alignment2 : Unbounded_String;

begin
   -- Read sequences
   Get_Line(Seq1);
   Get_Line(Seq2);
   
   -- Read gap penalties
   Get(Gap_Open);
   Get(Gap_Extend);
   
   -- Read scoring matrix (assuming 20x20 for amino acids)
   Score_Matrix := Read_Scoring_Matrix(20);
   
   -- Compute alignment
   Compute_Global_Alignment(Seq1, Seq2, Score_Matrix, Gap_Open, Gap_Extend, Score, Alignment1, Alignment2);
   
   -- Output results
   Put_Line(Integer'Image(Score));
   Put_Line(To_String(Alignment1));
   Put_Line(To_String(Alignment2));
   
end Global_Alignment_with_Scoring_Matrix_and_Affine_Gap_Penalty;
```

## Explanation

This Ada solution implements the global sequence alignment algorithm with affine gap penalties:

### Key Components:

1. **Data Structures**:
   - Uses 2D arrays for dynamic programming matrices (F, E, H)
   - F matrix stores optimal scores
   - E and H matrices track horizontal and vertical gap costs
   - Traceback matrix helps reconstruct the alignment

2. **Affine Gap Penalty Model**:
   - Gap opening penalty: Gap_Open
   - Gap extension penalty: Gap_Extend
   - Uses three matrices to handle different gap states

3. **Dynamic Programming**:
   - F(i,j) = max of match/mismatch, horizontal gap, vertical gap
   - E(i,j) and H(i,j) handle gap extensions
   - Proper initialization for first row/column

4. **Backtracking**:
   - Reconstructs alignment by following traceback directions
   - Handles matches, gaps in both sequences correctly

### Time Complexity: O(m×n)
### Space Complexity: O(m×n)

Where m and n are the lengths of the two input sequences.

The solution handles the specific requirements of Rosalind's global alignment problem with scoring matrices and affine gap penalties, providing both the optimal score and the actual aligned sequences.